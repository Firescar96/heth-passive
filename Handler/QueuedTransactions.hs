{-# LANGUAGE OverloadedStrings #-}

module Handler.QueuedTransactions where

import Blockchain.Data.DataDefs
import Handler.Common
import Handler.JsonJuggler
import Import
import qualified Prelude as P

getQueuedTransactionsR :: Handler Value
getQueuedTransactionsR  = do
    addHeader "Access-Control-Allow-Origin" "*"
    addr <- runDB $ selectList [ RawTransactionBlockNumber ==. (-1) ]
           [ LimitTo (fromIntegral $ fetchLimit :: Int), Desc RawTransactionNonce  ] :: Handler [Entity RawTransaction]
    returnJson $ P.map rtToRtPrime' (P.map entityVal (addr :: [Entity RawTransaction]))

