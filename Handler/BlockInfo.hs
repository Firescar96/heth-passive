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

module Handler.BlockInfo where

import Import

import Handler.Common 
import Blockchain.Data.DataDefs
import Blockchain.Data.Address

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Numeric
import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util


import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BS

import qualified Database.Esqueleto as E
       
import Data.List

import Control.Monad

import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import System.Locale
import Data.Time
import Data.Time.Format


import Blockchain.Data.Address
import Blockchain.ExtWord
import Numeric

import Yesod.Core.Handler

import Debug.Trace
import Handler.JsonJuggler

import Control.Monad
import Data.Bool

import Handler.Filters

import Data.Set

blockIdRef :: (E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity Block))-> expr (E.Value Bool)
blockIdRef (a, t) = (a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)
                    
if' :: Bool -> a -> b -> Either a b
if' x a b = if x == True then Left a else Right b

getBlockInfoR :: Handler Value
getBlockInfoR = do
                   getParameters <- reqGetParams <$> getRequest
                   
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
                   let newindex = pack $ show $ 1+(getNum $ P.last modBlocks)
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
                   
toParam a = Param a
fromParam (Param a) = a

data Param = Param (Text,Text)
instance Eq Param where
  Param a == Param b = fst a == fst b
instance Ord Param where
  (Param a) `compare` (Param b) = (fst a) `compare` (fst b)

appendIndex :: [(Text, Text)] -> [(Text,Text)] -- this sould be using URL encoding code from Yesod
appendIndex l = P.map fromParam (Data.Set.toList $ Data.Set.insert (toParam ("index", "")) $ Data.Set.fromList $ P.map toParam l)

extraFilter :: (Text,Text) -> Text -> (Text,Text)
extraFilter ("index", v) v' = ("index", v')
extraFilter (a,b) v'        = (a,b)

getNum:: Block -> Integer
--getNum (Block (BlockData{number = n})) = n
getNum (Block (BlockData ph uh cb@(Address a) sr tr rr lb d num gl gu ts ed non mh) rt bu) = num 

getSR:: Block -> String
getSR (Block (BlockData ph uh cb@(Address a) sr tr rr lb d num gl gu ts ed non mh) rt bu) = show sr 

getLengthOfBlocks :: [Block] -> Integer -> (Bool, Integer)
getLengthOfBlocks x n = ((P.length x) == fromIntegral n, fromIntegral $ P.length x)

getFirstBlockNum :: [Block] -> Integer -> (Bool, Integer)
getFirstBlockNum (x:xs) n = (getNum x == n, getNum x)

getLastBlockNum :: [Block] -> Integer -> (Bool, Integer)
getLastBlockNum x n = getFirstBlockNum (P.reverse x) n

getFirstBlockSR :: [Block] -> String -> (Bool, String)
getFirstBlockSR (x:xs) n = (getSR x == n, getSR x) 
