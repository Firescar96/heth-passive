{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module Handler.BlkId where
       
import Blockchain.Data.DataDefs
import Data.List
import Database.Persist.Postgresql
import Handler.JsonJuggler
import Import
import qualified Database.Esqueleto as E
import qualified Prelude as P

getBlkIdR :: Integer -> Handler Value
getBlkIdR n = do
    addHeader "Access-Control-Allow-Origin" "*"
    blks <- runDB $ E.select $ --Distinct
        E.from $ \(a, t) -> do
        E.where_ ( (a E.^. BlockDataRefBlockId E.==. E.val (toSqlKey (fromIntegral $ n)) ) E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)) ---- :: Handler [Entity AddressStateRef]
        return a
    returnJson $ nub $ P.map bdrToBdrPrime (P.map entityVal (blks :: [Entity BlockDataRef])) -- consider removing nub - it takes time n^{2}
