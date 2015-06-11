{-# LANGUAGE OverloadedStrings #-}

module Handler.BlkCoinbase where
import Import


import Handler.Common

import qualified Prelude as P
import Blockchain.Data.Address
import Blockchain.ExtWord
import Numeric
import Blockchain.Data.DataDefs

import qualified Database.Esqueleto as E

import qualified Data.Text as T

import Handler.JsonJuggler
import Data.List       

getBlkCoinbaseR :: Text -> Handler Value
getBlkCoinbaseR address = (getBlkCoinbaseR' address 0)

getBlkCoinbaseR' :: Text -> Integer -> Handler Value
getBlkCoinbaseR' address offset = do
    addHeader "Access-Control-Allow-Origin" "*"
    blks <- runDB $ E.select $
        E.from $ \(blk `E.InnerJoin` bdRef) -> do

        E.on ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId )                                        
        E.where_ (( bdRef E.^. BlockDataRefCoinbase E.==. E.val (Address wd160)) )

        E.limit $ (limit)
        E.offset $ (limit * off)

        E.orderBy [E.desc (bdRef E.^. BlockDataRefNumber)]

        return blk
    returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) 
        where
            ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]
            limit = (fromIntegral $ fetchLimit :: Int64)
            off = (fromIntegral $ offset :: Int64)

getBlkCoinbaseNumR :: Text -> Handler Value
getBlkCoinbaseNumR address = do
    addHeader "Access-Control-Allow-Origin" "*"
    count <- runDB $ E.select $
        E.from $ \a -> do
        E.where_ ( a E.^. BlockDataRefCoinbase E.==. E.val (Address wd160) )
        let cnt = E.countRows :: E.SqlExpr (E.Value Int) 
        return cnt
    returnJson $ P.map E.unValue count
        where ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]


-- data BdrPWNum = BdrPWNum BlockDataRef' Int
-- instance ToJSON BdrPWNum where
--     toJSON (BdrPWNum xx@(BlockDataRef' (BlockDataRef ph uh cb@(Address a) sr tr rr lb d num gl gu ts ed non mh bi h)) n) = 
--         object ["parentHash" .= ph, "unclesHash" .= uh, "coinbase" .= (showHex a ""), "stateRoot" .= sr,
--         "transactionsRoot" .= tr, "receiptsRoot" .= rr, "difficulty" .= d, "number" .= num,
--         "gasLimit" .= gl, "gasUsed" .= gu, "timestamp" .= ts, "extraData" .= ed, "nonce" .= non,
--         "mixHash" .= mh, "blockId" .= bi, "hash" .= h, "numQuery" .= n]

-- btobn (b, n) = BdrPWNum (bdrToBdrPrime b) n

-- untuple (a, b) = (entityVal a, E.unValue b)

-- getBlkCoinbaseR' :: Text -> Handler Value
-- getBlkCoinbaseR' address = do
--                            addHeader "Access-Control-Allow-Origin" "*"
--                            addr <- runDB $ E.select $
--                                 E.from $ \(a,t) -> do
--                                   E.where_ ( a E.^. BlockDataRefCoinbase E.==. E.val (Address wd160) )-- E.&&.( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
--                                   let cnt = E.countRows :: E.SqlExpr (E.Value Int) 
--                                   E.limit fetchLimit
--                                   E.groupBy $ a E.^. BlockDataRefId
--                                   --bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId 
--                                   E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
--                                   return (a, cnt)
--                            returnJson $ P.map btobn (P.map untuple (addr :: [(Entity BlockDataRef, E.Value Int)]))
--                            --returnJson $ P.map (bdrToBdrPrime) (P.map entityVal (addr :: [(Entity BlockDataRef)]))
--                            --returnJson $ P.map (bdrToBdrPrime . fst, snd) (P.map untuple (addr :: [(Entity BlockDataRef, E.Value Int)])) 
--                            where ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]
