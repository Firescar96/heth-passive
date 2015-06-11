{-# LANGUAGE OverloadedStrings #-}

module Handler.TxLast where

import Blockchain.Data.DataDefs
import Handler.Common
import Handler.JsonJuggler
import Import
import qualified Database.Esqueleto as E
import qualified Prelude as P

-- Parses addresses from hex 
getTxLastR :: Handler Value     
getTxLastR = (getTxLastR' 1)

getTxLastR' ::  Integer -> Handler Value
getTxLastR' num = do
    addHeader "Access-Control-Allow-Origin" "*"
    tx <- runDB $ E.select $
        E.from $ \rawTX -> do
        E.limit $ P.min (fromIntegral num :: Int64) fetchLimit 
        --   E.offset $ (limit * off)
        E.orderBy [E.desc (rawTX E.^. RawTransactionBlockId)]  
        return rawTX
    returnJson $ P.map rtToRtPrime' (P.map entityVal (tx :: [Entity RawTransaction]))
