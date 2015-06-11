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

module Handler.BlkTxAddress where

-- import Debug.Trace
import Blockchain.Data.Address
import Blockchain.Data.DataDefs
import Blockchain.ExtWord
import Data.List
import Handler.Common 
import Handler.JsonJuggler
import Import
import Numeric
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Prelude as P

getBlkTxAddressR :: Text -> Handler Value
getBlkTxAddressR a = (getBlkTxAddressR' a 0)

getBlkTxAddressR' :: Text -> Integer -> Handler Value
getBlkTxAddressR' address offset = do
    addHeader "Access-Control-Allow-Origin" "*"
    blks <- runDB $ E.select $
        E.from $ \(blk `E.InnerJoin` bdRef `E.FullOuterJoin` rawTX) -> do

            E.on ( bdRef E.^. BlockDataRefBlockId E.==. rawTX E.^. RawTransactionBlockId ) 
            E.on ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId )                                        
            E.where_ (( bdRef E.^. BlockDataRefCoinbase E.==. E.val (Address wd160)) E.||.
                      ( ( rawTX E.^. RawTransactionFromAddress E.==. E.val (Address wd160))
                          E.||. ( rawTX  E.^. RawTransactionToAddress E.==. E.val (Just (Address wd160) ))))

            E.limit $ (fetchLimit)
            E.offset $ (limit * off)

            E.orderBy [E.desc (bdRef E.^. BlockDataRefNumber)]

            return blk
    returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) 
        where
          ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]
          limit = (fromIntegral $ fetchLimit :: Int64)
          off = (fromIntegral $ offset :: Int64)
                    
