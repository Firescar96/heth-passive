{-# LANGUAGE DeriveDataTypeable
           , EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
           , GADTs
 #-}

module Handler.AccountInfo where
       
import Blockchain.Data.DataDefs
import Data.List
import Handler.Common 
import Handler.Filters
import Handler.JsonJuggler
import Import
import qualified Database.Esqueleto as E
import qualified Prelude as P

getAccountInfoR :: Handler Value
getAccountInfoR = do
    getParameters <- reqGetParams <$> getRequest
 
    appNameMaybe <- lookupGetParam "appname"
    case appNameMaybe of
        (Just t) -> liftIO $ putStrLn $ t
        (Nothing) -> liftIO $ putStrLn "anon"
    let offset = (fromIntegral $ (maybe 0 id $ extractPage "page" getParameters)  :: Int64)
    let index = (fromIntegral $ (maybe 0 id $ extractPage "index" getParameters)  :: Integer)

    -- liftIO $ traceIO $ "parameters: " P.++ show getParameters
    -- liftIO $ traceIO $ "index: " P.++ show index
    -- liftIO $ traceIO $ "offset: " P.++ show offset

    addHeader "Access-Control-Allow-Origin" "*"

    addrs <- runDB $ E.selectDistinct $
        E.from $ \(accStateRef) -> do
            E.where_ ((P.foldl1 (E.&&.) $ P.map (getAccFilter (accStateRef)) $ getParameters ))

            E.offset $ (limit * offset)
            E.limit $ limit

            E.orderBy [E.desc (accStateRef E.^. AddressStateRefBalance)]

            return accStateRef
    --liftIO $ traceIO $ "number of results: " P.++ (show $ P.length addrs)
    returnJson $ nub $ P.map asrToAsrPrime (P.map entityVal (addrs :: [Entity AddressStateRef])) -- consider removing nub - it takes time n^{2}
        where 
            limit = (fromIntegral $ fetchLimit :: Int64)
          
