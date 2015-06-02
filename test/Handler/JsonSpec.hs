{-# LANGUAGE QuasiQuotes #-}

module Handler.JsonSpec (spec) where

import TestImport

import qualified Test.HUnit as HUnit
import Network.Wai.Test

import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text.Lazy as TL
import qualified Data.List as DL
import Data.Aeson
import Data.Maybe

import System.Exit (exitFailure)
import System.IO (withFile, IOMode(..))

import Yesod.Test

import Handler.Common 
import Blockchain.Data.DataDefs

import Handler.BlockInfo

import Debug.Trace
mydebug = flip trace

contains :: BSL8.ByteString -> String -> Bool
contains a b = DL.isInfixOf b (TL.unpack $ decodeUtf8 a)

bodyContains' :: String -> YesodExample site ()
bodyContains' text = withResponse $ \ res ->
  liftIO $ HUnit.assertBool ("Expected body to contain " ++ text) $
    (simpleBody res) `contains` text



-- testJSONObject :: (Show a, Eq a) => ([Block] -> a -> Bool) -> a -> YesodExample site ()
-- testJSONObject n = withResponse $ \ res ->
--   liftIO $ HUnit.assertBool ("Expected JSON to parse " ++ show n) $

--    myobj res == n
--    where myobj res = getFirstBlockNum (fromJust (decode (simpleBody res) :: Maybe [Block]))


testJSON :: (Show a, Eq a) => ([Block] -> a -> (Bool, a)) -> a -> YesodExample site ()
testJSON f n = withResponse $ \ res ->
  liftIO $ HUnit.assertBool ("Compared JSON contents: " ++ show (snd (f (fromJust $ (decode (simpleBody res) :: Maybe [Block])) n)) ++ " and " ++ show n) $
    fst $ (f (fromJust $ (decode (simpleBody res) :: Maybe [Block])) n)

--genesis = (decode "[{\"blockUncles\":[],\"receiptTransactions\":[],\"blockData\":{\"logBloom\":\"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\",\"extraData\":0,\"gasUsed\":0,\"gasLimit\":3141592,\"unclesHash\":\"1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"mixHash\":\"0000000000000000000000000000000000000000000000000000000000000000\",\"receiptsRoot\":\"56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"number\":0,\"difficulty\":131072,\"timestamp\":\"1970-01-01T00:00:00.000Z\",\"coinbase\":{\"address\":\"0\"},\"parentHash\":\"0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":42,\"stateRoot\":\"9178d0f23c965d81f0834a4c72c6253ce6830f4022b1359aaebfc1ecba442d4e\",\"transactionsRoot\":\"56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\"}}]" ):: Maybe [Block]

checkKeyValue k v = withResponse $ \ SResponse { simpleHeaders = h } ->
                         liftIO $ HUnit.assertBool ("Value should be " ++ (show v)) $
                         fromJust (lookup k h) == v




spec :: Spec
spec = withApp $
  describe "Account endpoints" $ do
     it "First account" $ do
        get $ AccAddressR "1c11aa45c792e202e9ffdc2f12f99d0d209bef70"
        statusIs 200
        bodyContains' "contractRoot"
  --describe "Block endpoints" $ do
     it "Genesis block" $ do
        get $ ("/query/block?number=0&raw=1" :: Text)
        statusIs 200 
        bodyContains' "9178d0f23c965d81f0834a4c72c6253ce6830f4022b1359aaebfc1ecba442d4e"
        --testJSON  getFirstBlockSR "9178d0f23c965d81f0834a4c72c6253ce6830f4022b1359aaebfc1ecba442d4e"
        testJSON getFirstBlockNum 0
  --describe "Paging " $ do
     it "Paging" $ do --reqGetParams <$> getRequest
        get ("/query/block?number=100" :: Text)
        statusIs 200
        bodyContains' "\"number\":100"
     it "Indexing" $ do
        get ("/query/block?maxnumber=100&index=51&raw=1" :: Text)
        statusIs 200
        testJSON getLengthOfBlocks 50
     it "Indexing empty" $ do
        get ("/query/block?maxnumber=50&index=51&raw=1" :: Text)
        statusIs 200
        testJSON getLengthOfBlocks 0

     it "First block through inequalities" $ do
        get ("/query/block?maxnumber=0&minnumber=0&raw=1" :: Text)
        statusIs 200
        testJSON getLengthOfBlocks 1
        testJSON getFirstBlockNum 0

     it "Transaction from block" $ do
        get ("/query/transaction?blockid=0&raw=1" :: Text)
        statusIs 200

     it "Access pattern" $ do
        get ("/query/block?minnumber=0&maxnumber=50&index=0&raw=1" :: Text)
        checkKeyValue "Access-Control-Allow-Origin" "*"

     it "Content type" $ do
        get ("/query/block?minnumber=0&maxnumber=50&index=0&raw=1" :: Text)
        checkKeyValue "Content-Type" "application/json; charset=utf-8"

     it "Last of previous index is one less than next index" $ do
        get $ qsIndex s 0
        statusIs 200
        n1 <- withResponse $ \ res -> do 
          return $ snd $ (getFirstBlockNum (fromJust $ (decode (simpleBody res) :: Maybe [Block])) 0)
        get $ qsIndex s n1
        testJSON getLengthOfBlocks 100
        n2 <- withResponse $ \ res -> do 
          return $ snd $ (getFirstBlockNum (fromJust $ (decode (simpleBody res) :: Maybe [Block])) 0)
        liftIO $ HUnit.assertBool("N+1: ") $ n1 == n2
        where s = "/query/block?minnumber=1&maxnumber=201&raw=1"
              qsIndex s n = s ++ "&index=" ++ (show  n)

     -- it "Next"
     --    get ("/query/block?minnumber=1&maxnumber=200" :: Text)
     --    statusIs 200
     --    n1 <- withResponse $ \res -> do
     --        return $