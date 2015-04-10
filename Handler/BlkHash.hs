module Handler.BlkHash where

import Import

import Data.Aeson
import qualified Blockchain.Data.DataDefs as DD
import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util

import Data.ByteString.Base16 as B16
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import qualified Database.Esqueleto as E

import Handler.PQuery
import Data.List
import qualified Data.Text.Encoding as T        
import qualified Prelude as P


getBlkHashR :: Text -> Handler Value
getBlkHashR h =                 do addHeader "Access-Control-Allow-Origin" "*"
                                   blks <- runDB $ E.selectDistinct $
                                        E.from $ \(a, t) -> do
                                        E.where_ ( (a E.^. BlockRefHash E.==. E.val ( DD.SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ h ) ) E.&&. ( a E.^. BlockRefBlockId E.==. t E.^. BlockId))
                                        return t
                                   returnJson $ nub $ (P.map entityVal blks) -- consider removing nub - it takes time n^{2}
