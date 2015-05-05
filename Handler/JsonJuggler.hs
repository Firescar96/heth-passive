{-# LANGUAGE OverloadedStrings #-}


module Handler.JsonJuggler where

import Import

import Handler.Common 
import Blockchain.Data.DataDefs
import Blockchain.Data.Address
import Blockchain.Data.PersistTypes

import Data.Aeson
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import qualified Database.Esqueleto as E

import Numeric
import Data.List
import Debug.Trace

import Prelude as P

jsonBlk :: (ToJSON a, Monad m) => a -> m Value
jsonBlk a = returnJson a


data RawTransaction' = RawTransaction' RawTransaction deriving (Eq, Show)

instance ToJSON RawTransaction' where
    toJSON (RawTransaction' (RawTransaction fa non gp gl ta val cod v r s bid)) =
        object ["from" .= adToAdPrime fa, "nonce" .= non, "gasPrice" .= gp, "gasLimit" .= gl,
        "to" .= fmap adToAdPrime ta , "value" .= val, "codeOrData" .= cod, "v" .= v, "r" .= r, "s" .= s,
        "blockId" .= bid]

rtToRtPrime :: RawTransaction -> RawTransaction'
rtToRtPrime x = RawTransaction' x

tToTPrime = P.id

data Block' = Block' Block deriving (Eq, Show)

instance ToJSON Block' where
      toJSON (Block' (Block bd rt bu)) = --"hello"
        object ["blockData" .= bdToBdPrime bd,
         "receiptTransactions" .= P.map tToTPrime rt,
         "blockUncles" .= P.map bdToBdPrime bu]

bToBPrime :: Block -> Block'
bToBPrime x = Block' x

data BlockData' = BlockData' BlockData deriving (Eq, Show)

instance ToJSON BlockData' where
      toJSON (BlockData' (BlockData ph uh cb@(Address a) sr tr rr lb d num gl gu ts ed non mh)) = 
        object ["parentHash" .= ph, "unclesHash" .= uh, "coinbase" .= (showHex a ""), "stateRoot" .= sr,
        "transactionsRoot" .= tr, "receiptsRoot" .= rr, "difficulty" .= d, "number" .= num,
        "gasLimit" .= gl, "gasUsed" .= gu, "timestamp" .= ts, "extraData" .= ed, "nonce" .= non,
        "mixHash" .= mh]

bdToBdPrime :: BlockData -> BlockData'
bdToBdPrime x = BlockData' x

data BlockDataRef' = BlockDataRef' BlockDataRef deriving (Eq, Show)

instance ToJSON BlockDataRef' where
      toJSON (BlockDataRef' (BlockDataRef ph uh cb@(Address a) sr tr rr lb d num gl gu ts ed non mh bi h)) = 
        object ["parentHash" .= ph, "unclesHash" .= uh, "coinbase" .= (showHex a ""), "stateRoot" .= sr,
        "transactionsRoot" .= tr, "receiptsRoot" .= rr, "difficulty" .= d, "number" .= num,
        "gasLimit" .= gl, "gasUsed" .= gu, "timestamp" .= ts, "extraData" .= ed, "nonce" .= non,
        "mixHash" .= mh, "blockId" .= bi, "hash" .= h]


bdrToBdrPrime :: BlockDataRef -> BlockDataRef'
bdrToBdrPrime x = BlockDataRef' x

data AddressStateRef' = AddressStateRef' AddressStateRef deriving (Eq, Show)

instance ToJSON AddressStateRef' where
    toJSON (AddressStateRef' (AddressStateRef a@(Address x) n b cr ch)) = 
        object ["address" .= (showHex x ""), "nonce" .= n, "balance" .= b, 
        "contractRoot" .= cr, "codeHash" .= ch]

asrToAsrPrime :: AddressStateRef -> AddressStateRef'
asrToAsrPrime x = AddressStateRef' x

--jsonFix x@(AddressStateRef a b c d e) = AddressStateRef' x
--jsonFix x@(BlockDataRef a b c d e f g h i j k l m n o p q) = BlockDataRef' x


data Address' = Address' Address deriving (Eq, Show)
adToAdPrime x = Address' x

instance ToJSON Address' where
  toJSON (Address' x) = object [ "address" .= (showHex x "") ]