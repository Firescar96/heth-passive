module Handler.BlkHash where

import Blockchain.Data.DataDefs
import Blockchain.SHA
import Blockchain.Util
import Data.ByteString.Base16 as B16
import Data.List
import Handler.JsonJuggler
import Import
import qualified Data.Text.Encoding as T        
import qualified Database.Esqueleto as E
import qualified Prelude as P

getBlkHashR :: Text -> Handler Value
getBlkHashR h = do 
    addHeader "Access-Control-Allow-Origin" "*"
    blks <- runDB $ E.selectDistinct $
        E.from $ \(a, t) -> do
        E.where_ ( (a E.^. BlockDataRefHash E.==. E.val ( SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ h ) ) E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
        return t
    returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}

