{-# LANGUAGE DeriveDataTypeable
           , EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
           , GADTs
           , OverloadedStrings
 #-}

module Handler.BlockInfo where

import Blockchain.Data.DataDefs
import Data.List
import Handler.Common
import Handler.Filters
import Handler.JsonJuggler
import Import

import qualified Database.Esqueleto as E
import qualified Prelude as P

blockIdRef :: (E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity Block))-> expr (E.Value Bool)
blockIdRef (a, t) = (a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)
                    

getBlockInfoR :: Handler Value
getBlockInfoR = do
    getParameters <- reqGetParams <$> getRequest

    appNameMaybe <- lookupGetParam "appname" 
    case appNameMaybe of
        (Just t) -> liftIO $ putStrLn $ t
        (Nothing) -> liftIO $ putStrLn "anon"
     
    let offset = (fromIntegral $ (maybe 0 id $ extractPage "page" getParameters)  :: Int64)
    let index  = (fromIntegral $ (maybe 0 id $ extractPage "index" getParameters)  :: Integer)
    let raw    = (fromIntegral $ (maybe 0 id $ extractPage "raw" getParameters) :: Integer) > 0

    -- liftIO $ traceIO $ "parameters: " P.++ show getParameters
    -- liftIO $ traceIO $ "index: " P.++ show index
    -- liftIO $ traceIO $ "offset: " P.++ show offset
    -- liftIO $ traceIO $ "raw: " P.++ show raw

    addHeader "Access-Control-Allow-Origin" "*"

    blks <- runDB $ E.select $
                            
        E.from $ \(blk `E.InnerJoin` bdRef `E.FullOuterJoin` rawTX `E.LeftOuterJoin` accStateRef) -> do
        
        E.on ( accStateRef E.^. AddressStateRefAddress E.==. rawTX E.^. RawTransactionFromAddress )
        E.on ( rawTX E.^. RawTransactionBlockId E.==. bdRef E.^. BlockDataRefBlockId )
        E.on ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId )                                        

        let criteria = P.map (getBlkFilter (bdRef, accStateRef, rawTX, blk)) $ getParameters 
        let allCriteria = ((bdRef E.^. BlockDataRefNumber) E.>=. E.val index) : criteria

        E.where_ (P.foldl1 (E.&&.) allCriteria)

        E.offset $ (limit * offset)
        E.limit $ limit

        E.orderBy [E.asc (bdRef E.^. BlockDataRefNumber)]

        return blk
        --liftIO $ traceIO $ "number of results: " P.++ (show $ P.length blks)

    let modBlocks = (nub (P.map entityVal (blks :: [Entity Block])))
    let newindex = pack $ show $ 1+(getBlockNum $ P.last modBlocks)
    let extra p = P.zipWith extraFilter p (P.repeat (newindex))
    -- this should actually use URL encoding code from Yesod
    let next p = "/query/block?" P.++  (P.foldl1 (\a b -> (unpack a) P.++ "&" P.++ (unpack b)) $ P.map (\(k,v) -> (unpack k) P.++ "=" P.++ (unpack v)) (extra p))
    let addedParam = appendIndex getParameters

    toRet raw modBlocks (next addedParam) -- consider removing nub - it takes time n^{2}
    where
        toRet raw bs gp = case if' raw bs (P.map bToBPrime (P.zip (P.repeat gp) bs)) of 
            Left a -> returnJson a
            Right b -> returnJson b
        limit = (fromIntegral $ fetchLimit :: Int64)
                   
