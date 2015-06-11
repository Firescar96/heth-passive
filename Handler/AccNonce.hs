module Handler.AccNonce where

import Blockchain.Data.DataDefs
import Data.List
import Handler.Common
import Handler.JsonJuggler
import Import
import qualified Database.Esqueleto as E
import qualified Prelude as P
       
-- Parses addresses from hex      
getAccNonceR :: Integer -> Handler Value
getAccNonceR nonce =   do
    addHeader "Access-Control-Allow-Origin" "*"
    blks <- runDB $ E.selectDistinct $
        E.from $ \(acc) -> do
            E.where_ ( (acc E.^. AddressStateRefNonce E.==. E.val nonce ) )
            E.limit $ fetchLimit
            return acc
    returnJson $ nub $ P.map asrToAsrPrime (P.map entityVal blks :: [AddressStateRef]) -- consider removing nub - it takes time n^{2}


