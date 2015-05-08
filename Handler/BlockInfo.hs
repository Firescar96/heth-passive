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

import Handler.Filters

blockIdRef :: (E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity Block))-> expr (E.Value Bool)
blockIdRef (a, t) = (a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)

getBlockInfoR :: Handler Value
getBlockInfoR = do
  	           getParameters <- reqGetParams <$> getRequest
                   liftIO $ traceIO $ show getParameters
                   addHeader "Access-Control-Allow-Origin" "*"
                   blks <- runDB $ E.select $
                                        E.from $ \(blk `E.InnerJoin` bdRef `E.FullOuterJoin` rawTX `E.LeftOuterJoin` accStateRef) -> do
                                        
                                        E.on ( accStateRef E.^. AddressStateRefAddress E.==. rawTX E.^. RawTransactionFromAddress )
                                        E.on ( rawTX E.^. RawTransactionBlockId E.==. bdRef E.^. BlockDataRefBlockId )
                                        E.on ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId )                                        


                                        E.where_ ((P.foldl1 (E.&&.) $ P.map (getBlkFilter (bdRef, accStateRef, rawTX, blk)) $ getParameters ))

                                        E.limit $ (fetchLimit)

                                        E.orderBy [E.desc (bdRef E.^. BlockDataRefNumber)]

                                        return blk
                   returnJson $ nub $ P.map bToBPrime (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}


    {-

do
  addHeader "Access-Control-Allow-Origin" "*"
  blks <- runDB $ E.selectDistinct $
                                        E.from $ \x@(_, t) -> do
                                        E.where_ (
                                          P.foldl1 (E.&&.)     [getFilter x "number", getFilter x "blockId"])
                                        return t
  returnJson $ nub $ (P.map entityVal blks) -- consider removing nub - it takes time n^{2}
-}
