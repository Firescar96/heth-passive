module Handler.BlkTimeRange where

import Blockchain.Data.DataDefs
import Data.List
import Handler.Common
import Handler.JsonJuggler
import Import
import qualified Database.Esqueleto as E
import qualified Prelude as P

getBlkTimeRangeR :: UTCTime -> UTCTime -> Handler Value
getBlkTimeRangeR g1 g2 = do 
    addHeader "Access-Control-Allow-Origin" "*"
    blks <- runDB $ E.select $
        E.from $ \(blk `E.InnerJoin` bdRef) -> do
        E.on ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId )   
        E.where_ (( (bdRef E.^. BlockDataRefTimestamp E.>=. E.val g1 ) E.&&. (bdRef E.^. BlockDataRefTimestamp E.<=. E.val g2)) )--  E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId))
       
        E.orderBy [E.desc (bdRef E.^. BlockDataRefNumber)]
        E.limit $ fetchLimit
        return blk
    returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}
