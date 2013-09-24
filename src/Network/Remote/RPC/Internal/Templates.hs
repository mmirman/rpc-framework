{-# LANGUAGE
 TemplateHaskell,
 PatternGuards
 #-}
{-
Module      :  Network.Remote.RPC.Internal.Templates
Copyright   :  (c) Matthew Mirman 2012
License     :  BSD-style (see the file LICENSE)
Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
Stability   :  experimental
Portability :  TemplateHaskell, PatternGuards

The functions for easily making a remote procedure call and setting up servers
-}
module Network.Remote.RPC.Internal.Templates ( rpcCall
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

-- | @$('makeHost' \"HostName\" \"hostLocation\" hostPortNumber)@ 
-- makes a @newtype HostName@ and declares an @instance 'Host' HostName@.
makeHost :: String -> String -> Integer -> Q [Dec]
makeHost n l p = do
  let nm = mkName n
  let host = mkName "Host"
  dat <- dataD (cxt []) nm [] [normalC nm []] []
  sh <- instanceD (cxt []) (appT (conT $ mkName "Show") (conT nm))
        [ funD (mkName "show") [clause [wildP] (normalB $ stringE n) []]
        ]  
  inst <- instanceD (cxt []) (appT (conT host) (conT nm))
          [ funD (mkName "getDataDefault") [clause [wildP] (normalB $ tupE [ stringE l
                                                                           , litE $ IntegerL p
                                                                           ]) []]
          , funD (mkName "getValue") [clause [] (normalB $ conE nm) []]
          ]
  return [ dat
         , sh
         , inst
         ]

-- | @$('rpcCall' 'serviceNm)@ simply splices in 
-- @'realRemoteCall' (undefined :: typeofcall) \"serviceNm\"@ which is typed in a manner
-- similar to typeofcall. 
rpcCall :: Name -> Q Exp
rpcCall name = do
  VarI _ ty _ _ <- reify name
  let nm = show name
      
  appsE [ varE 'realRemoteCall -- realRemoteCall
        , sigE (varE 'undefined) $ return ty -- (undefined :: ty)
        , stringE nm -- nm
        ]

-- | @$('makeServices' ['service1 ,..., 'serviceN])@ makes all given 
-- services listen for incoming requests.
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

-- | @$('autoService' 'World)@ finds all services declared in the 
-- module that definitely run on the given world, 
-- and makes them listen for incoming requests.
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
