{-# LANGUAGE OverloadedStrings #-}

module Handler.AccAddress where
import Import
       
import Blockchain.Data.DataDefs
--import Blockchain.ExtWord
import Handler.Common
import Handler.Filters
import Handler.JsonJuggler
--import Numeric
--import qualified Data.Text as T
import qualified Prelude as P

-- Parses addresses from hex      

getAccAddressR :: Text -> Handler Value
getAccAddressR a = (getAccAddressR' a 0)

getAccAddressR' :: Text -> Integer -> Handler Value
getAccAddressR' address offset = do
    addHeader "Access-Control-Allow-Origin" "*"
    addr <- runDB $ selectList [ AddressStateRefAddress ==. (toAddr address) ] [ LimitTo limit , OffsetBy (limit * off) ] :: Handler [Entity AddressStateRef]
    returnJson $ P.map asrToAsrPrime (P.map entityVal (addr :: [Entity AddressStateRef])) -- consider removing nub - it takes time n^{2}
    where
        --((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]
        limit = (fromIntegral $ fetchLimit :: Int)
        off = (fromIntegral $ offset :: Int)
                    
