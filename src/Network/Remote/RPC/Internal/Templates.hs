{-# LANGUAGE
 TemplateHaskell,
 PatternGuards
 #-} 
module Network.Remote.RPC.Internal.Templates ( build
                                             , rpcCall
                                             , makeHost
                                             , makeServices
                                             , autoService
                                             ) where
import Control.Monad (forM)
import Data.Functor ((<$>))
import Language.Haskell.TH
import Control.Monad.IO.Class (MonadIO(..))
import Network.Remote.RPC.Internal.Runtime (realRemoteCall, makeService)
import Data.List (nub)

instance MonadIO Q where liftIO = runIO

makeHost :: String -> String -> Integer -> Q [Dec]
makeHost n l p = do
  let nm = mkName n
  let host = mkName "Host"
  dat <- dataD (cxt []) nm [] [normalC nm []] []
  inst <- instanceD (cxt []) (appT (conT host) (conT nm))
          [ funD (mkName "getLocation") [clause [wildP] (normalB $ stringE l) []]
          , funD (mkName "getPort") [clause [wildP] (normalB $ litE $ IntegerL p) []]
          , funD (mkName "getValue") [clause [] (normalB $ conE nm) []]
          ]
  return [ dat
         , inst
         ]

build :: Q [Dec] -> Q [Dec]
build m = do
  dlist <- m
  dl <- mapM act dlist
  return $ concat dl
  
act :: Dec -> Q [Dec]

act v = return [v]

rpcCall :: Name -> Q Exp
rpcCall name = do
  VarI _ ty _ _ <- reify name
  let nm = show name
      
  appsE [ varE 'realRemoteCall -- realRemoteCall
        , sigE (varE 'undefined) $ return ty -- (undefined :: ty)
        , stringE nm -- nm
        ]
  
makeServices :: [Name] -> Q Exp
makeServices names = do
  doE $ flip map names $ \nm -> 
    noBindS $ appsE [ varE 'makeService 
                    , varE nm
                    , stringE $ show nm
                    ]

extractAllFunctions :: String-> [String]
extractAllFunctions file  = 
  --  allMatchingFunctions pattern . parsedModule
  nub $ map (fst . head . lex) $ lines file

getHost :: Type -> Maybe Name
getHost t = case t of
  ForallT _ _ t -> getHost t
  AppT (AppT ArrowT _) t -> getHost t
  AppT (AppT (AppT (ConT wio) (ConT nm)) _) _ | "WIO" <- nameBase wio, nameModule wio == nameModule 'makeService -> Just nm
  _ -> Nothing

autoService :: Name -> Q Exp
autoService host = do
  file <- loc_filename <$> location
  moduleCode <- runIO $ readFile file
  nms <- forM (extractAllFunctions moduleCode) $ \nm -> recover (return []) $ do
    f <- reify $ mkName nm
    return $ case f of 
      VarI nm ty _ _ | show (getHost ty) == show (Just host) -> [nm]
      _ -> []

  makeServices $ concat nms
