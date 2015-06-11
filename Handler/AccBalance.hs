{-# LANGUAGE OverloadedStrings #-}


module Handler.AccBalance where

import Blockchain.Data.DataDefs
import Handler.Common
import Handler.JsonJuggler
import Import
import qualified Prelude as P
       
getAccBalanceR :: Integer -> Handler Value
getAccBalanceR n = do 
    addHeader "Access-Control-Allow-Origin" "*"
    acc <- runDB $ selectList [ AddressStateRefBalance ==. n ] [LimitTo (fromIntegral $ fetchLimit :: Int)] :: Handler [Entity AddressStateRef]   
    returnJson $ P.map asrToAsrPrime (P.map entityVal (acc :: [Entity AddressStateRef])) 



