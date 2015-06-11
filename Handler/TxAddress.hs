{-# LANGUAGE OverloadedStrings #-}

module Handler.TxAddress where

import Blockchain.Data.Address
import Blockchain.Data.DataDefs
import Blockchain.ExtWord
import Handler.Common
import Handler.JsonJuggler
import Import
import Numeric
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Prelude as P

-- Parses addresses from hex      
getTxAddressR :: Text -> Handler Value
getTxAddressR address = (getTxAddressR' address 0)

getTxAddressR' :: Text -> Integer -> Handler Value
getTxAddressR' address offset = do
    addHeader "Access-Control-Allow-Origin" "*"
    tx <- runDB $ E.select $
        E.from $ \rawTX -> do
            E.where_ (
                ((rawTX E.^. RawTransactionFromAddress E.==. E.val (Address wd160)))
                E.||. 
                    (rawTX E.^. RawTransactionToAddress E.==. E.val (Just (Address wd160)))
                )
            E.limit $ limit
            E.offset $ (limit * off)
            E.orderBy [E.desc (rawTX E.^. RawTransactionNonce)]  
            return rawTX
    returnJson $ P.map rtToRtPrime' (P.map entityVal (tx :: [Entity RawTransaction]))
    where
        ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]
        limit = (fromIntegral $ fetchLimit :: Int64)
        off = (fromIntegral $ offset :: Int64)
