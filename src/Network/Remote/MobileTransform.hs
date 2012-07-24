{-# LANGUAGE
 TemplateHaskell,
 MultiParamTypeClasses,
 ScopedTypeVariables
 #-} 
module Network.Remote.MobileTransform ( build
                                      , rpcCall
                                      , makeHost
                                      , makeServices
                                      ) where

import Language.Haskell.TH
import Control.Monad.IO.Class
import Network.Remote.RPCInternal


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
