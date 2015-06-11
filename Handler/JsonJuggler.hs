{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.JsonJuggler where

import Blockchain.Data.Address
import Blockchain.Data.Code
import Blockchain.Data.DataDefs
import Blockchain.Data.Transaction
import Data.Aeson
import Data.ByteString
import Data.ByteString.Base16 as B16
import Database.Persist.Postgresql
import Import
import Numeric
import Prelude as P
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T

jsonBlk :: (ToJSON a, Monad m) => a -> m Value
jsonBlk a = returnJson a

data RawTransaction' = RawTransaction' RawTransaction String deriving (Eq, Show)


instance ToJSON RawTransaction' where
    toJSON (RawTransaction' rt@(RawTransaction (Address fa) non gp gl (Just (Address ta)) val cod r s v _bid bn h) next) =
        object ["next" .= next, "from" .= showHex fa "", "nonce" .= non, "gasPrice" .= gp, "gasLimit" .= gl,
        "to" .= showHex ta "" , "value" .= show val, "codeOrData" .= cod, 
        "r" .= showHex r "",
        "s" .= showHex s "",
        "v" .= showHex v "",
        "blockNumber" .= bn,
        "hash" .= h,
        "transactionType" .= (show $ rawTransactionSemantics rt)
               ]
    toJSON (RawTransaction' rt@(RawTransaction (Address fa) non gp gl Nothing val cod r s v _bid bn h) next) =
        object ["next" .= next, "from" .= showHex fa "", "nonce" .= non, "gasPrice" .= gp, "gasLimit" .= gl,
        "value" .= show val, "codeOrData" .= cod,
        "r" .= showHex r "",
        "s" .= showHex s "",
        "v" .= showHex v "",
        "blockNumber" .= bn,
        "hash" .= h,
        "transactionType" .= (show $ rawTransactionSemantics rt)
               ]

instance FromJSON RawTransaction' where
    parseJSON (Object t) = do
        fa <- fmap (fst P.. P.head P.. readHex) (t .: "from")       --- ugly ! stupid Prelude qualified import 
        (tnon :: Int)  <- (t .: "nonce")
        (tgp :: Int) <- (t .: "gasPrice")
        (tgl :: Int) <- (t .: "gasLimit")
        tto <- (t .:? "to")
        let toFld = case tto of
                Just str -> fmap (Address P.. fst P.. P.head P.. readHex) str
                Nothing -> Nothing
        tval <- fmap read (t .: "value")
        tcd <- fmap (fst P..  B16.decode P.. T.encodeUtf8 ) (t .: "codeOrData")
        (tr :: Integer) <- fmap (fst P.. P.head P.. readHex) (t .: "r")
        (ts :: Integer) <- fmap (fst P.. P.head P.. readHex) (t .: "s")
        (tv :: Word8) <- fmap (fst P.. P.head P.. readHex) (t .: "v")
        mbid <- (t .:? "blockId")
        mbn <- (t .:? "blockNumber")
        h <- (t .: "hash")
        let bid = case mbid of
                Just bd -> bd
                Nothing -> 1 -- annoying, needs to be reset on update
        let bn = case mbn of
                Just b -> b
                Nothing -> -1

        return (RawTransaction' (RawTransaction (Address fa)
            (fromIntegral tnon :: Integer)
            (fromIntegral $ tgp :: Integer)
            (fromIntegral $ tgl :: Integer)
            (toFld :: Maybe Address)
            (tval :: Integer)
            (tcd :: B.ByteString)
            (tr :: Integer)
            (ts :: Integer)
            (tv :: Word8)
            (toSqlKey bid)
            bn
            h) "") 

    parseJSON _ = mzero


rtToRtPrime :: (String , RawTransaction) -> RawTransaction'
rtToRtPrime (s, x) = RawTransaction' x s

rtToRtPrime' :: RawTransaction -> RawTransaction'
rtToRtPrime' x = RawTransaction' x ""

data Transaction' = Transaction' Transaction deriving (Eq, Show)

instance ToJSON Transaction' where
    toJSON (Transaction' tx@(MessageTX tnon tgp tgl tto tval td tr ts tv)) = 
        object ["kind" .= ("Transaction" :: String), "nonce" .= tnon, "gasPrice" .= tgp, "gasLimit" .= tgl, "to" .= tto, "value" .= tval,
        "data" .= td, "r" .= showHex tr "", "s" .= showHex ts "", "v" .= showHex tv "",
        "transactionType" .= (show $ transactionSemantics $ tx)]
    toJSON (Transaction' tx@(ContractCreationTX tnon tgp tgl tval ti tr ts tv)) = 
        object ["kind" .= ("Transaction" :: String), "nonce" .= tnon, "gasPrice" .= tgp, "gasLimit" .= tgl, "value" .= tval, "init" .= ti,
        "r" .= showHex tr "", "s" .= showHex ts "", "v" .= showHex tv "",
        "transactionType" .= (show $ transactionSemantics $ tx)]


instance FromJSON Transaction' where
    parseJSON (Object t) = do
        tto <- (t .:? "to")
        tnon <- (t .: "nonce")
        tgp <- (t .: "gasPrice")
        tgl <- (t .: "gasLimit")
        tval <- (t .: "value")
        tr <- (t .: "r")
        ts <- (t .: "s")
        tv <- (t .: "v")

        case tto of
            Nothing -> do
                ti <- (t .: "init")
                return (Transaction' (ContractCreationTX tnon tgp tgl tval ti tr ts tv))
            (Just to) -> do
                td <- (t .: "data")
                return (Transaction' (MessageTX tnon tgp tgl to tval td tr ts tv))
    parseJSON _ = mzero    
{-        case res of
          Nothing -> Transaction' ( ContractCreationTX <$>
                     (t .: "nonce") <*>
                     (t .: "gasPrice") <*>
                     (t .: "gasLimit") <*>
                     (t .: "value") <*>
                     (t .: "init") <*>
                     (t .: "r") <*>
                     (t .: "s") <*>
                     (t .: "v") 
                    )
          _ ->      Transaction' ( MessageTX <$>
                     (t .: "nonce") <*>
                     (t .: "gasPrice") <*>
                     (t .: "gasLimit") <*>
                     (t .: "to" ) <*>   
                     (t .: "value") <*>
                     (t .: "data") <*>
                     (t .: "r") <*>
                     (t .: "s") <*>
                     (t .: "v") 
                    )
-}

tToTPrime :: Transaction -> Transaction'
tToTPrime x = Transaction' x

data Block' = Block' Block String deriving (Eq, Show)

instance ToJSON Block' where
    toJSON (Block' (Block bd rt bu) next) =
        object ["next" .= next, "kind" .= ("Block" :: String), "blockData" .= bdToBdPrime bd,
            "receiptTransactions" .= P.map tToTPrime rt,
            "blockUncles" .= P.map bdToBdPrime bu]

bToBPrime :: (String , Block) -> Block'
bToBPrime (s, x) = Block' x s


bToBPrime' :: Block -> Block'
bToBPrime' x = Block' x ""

data BlockData' = BlockData' BlockData deriving (Eq, Show)

instance ToJSON BlockData' where
    toJSON (BlockData' (BlockData ph uh _cb@(Address a) sr tr rr _lb d num gl gu ts ed non mh)) =
        object ["kind" .= ("BlockData" :: String), "parentHash" .= ph, "unclesHash" .= uh, "coinbase" .= (showHex a ""), "stateRoot" .= sr,
            "transactionsRoot" .= tr, "receiptsRoot" .= rr, "difficulty" .= d, "number" .= num,
            "gasLimit" .= gl, "gasUsed" .= gu, "timestamp" .= ts, "extraData" .= ed, "nonce" .= non,
            "mixHash" .= mh]

bdToBdPrime :: BlockData -> BlockData'
bdToBdPrime x = BlockData' x

data BlockDataRef' = BlockDataRef' BlockDataRef deriving (Eq, Show)

instance ToJSON BlockDataRef' where
    toJSON (BlockDataRef' (BlockDataRef ph uh _cb@(Address a) sr tr rr _lb d num gl gu ts ed non mh bi h pow isConf td)) = 
        object ["parentHash" .= ph, "unclesHash" .= uh, "coinbase" .= (showHex a ""), "stateRoot" .= sr,
            "transactionsRoot" .= tr, "receiptsRoot" .= rr, "difficulty" .= d, "number" .= num,
            "gasLimit" .= gl, "gasUsed" .= gu, "timestamp" .= ts, "extraData" .= ed, "nonce" .= non,
            "mixHash" .= mh, "blockId" .= bi, "hash" .= h, "powVerified" .= pow, "isConfirmed" .= isConf, "totalDifficulty" .= td]



bdrToBdrPrime :: BlockDataRef -> BlockDataRef'
bdrToBdrPrime x = BlockDataRef' x

data AddressStateRef' = AddressStateRef' AddressStateRef deriving (Eq, Show)

instance ToJSON AddressStateRef' where
    toJSON (AddressStateRef' (AddressStateRef _a@(Address x) n b cr ch)) = 
        object ["kind" .= ("AddressStateRef" :: String), "address" .= (showHex x ""), "nonce" .= n, "balance" .= show b, 
            "contractRoot" .= cr, "codeHash" .= ch]

asrToAsrPrime :: AddressStateRef -> AddressStateRef'
asrToAsrPrime x = AddressStateRef' x

--jsonFix x@(AddressStateRef a b c d e) = AddressStateRef' x
--jsonFix x@(BlockDataRef a b c d e f g h i j k l m n o p q) = BlockDataRef' x

data Address' = Address' Address deriving (Eq, Show)

adToAdPrime :: Address -> Address'
adToAdPrime x = Address' x

--instance ToJSON Address' where
--  toJSON (Address' x) = object [ "address" .= (showHex x "") ]

data TransactionType = Contract | FunctionCall | Transfer  deriving (Eq, Show)

--instance ToJSON TransactionType where 
--   toJSON x = object ["transactionType" .= show x]

transactionSemantics :: Transaction -> TransactionType
transactionSemantics _t@(MessageTX _tnon _tgp _tgl _tto@(Address _x) _tval td _tr _ts _tv) = work
    where work 
            | (B.length td) > 0 = FunctionCall
            | otherwise = Transfer
transactionSemantics _t@(ContractCreationTX _tnon _tgp _tgl _tval (Code _ti) _tr _ts _tv)
            | otherwise = Contract                                  

isAddr :: Maybe Address -> Bool
isAddr a = case a of
    Just _x   -> True
    Nothing  -> False

rawTransactionSemantics :: RawTransaction -> TransactionType
rawTransactionSemantics _t@(RawTransaction _fa _non _gp _gl ta _val cod _v _r _s _bid _bn _h) = work
    where work 
            | (not (isAddr ta))  = Contract
            | (isAddr ta) &&  ((Data.ByteString.length cod) > 0) = FunctionCall
            | otherwise = Transfer
