module Handler.BlkGasRange where
       
import Blockchain.Data.DataDefs
import Data.List
import Handler.Common
import Handler.JsonJuggler
import Import
import qualified Database.Esqueleto as E
import qualified Prelude as P

getBlkGasRangeR :: Integer -> Integer -> Handler Value
getBlkGasRangeR = (getBlkGasRangeR' 0)
       
getBlkGasRangeR' :: Integer -> Integer -> Integer-> Handler Value
getBlkGasRangeR' g1 g2 offset = do 
    addHeader "Access-Control-Allow-Origin" "*"
    blks <- runDB $ E.select $
        E.from $ \(a, t) -> do
        E.where_ ( (a E.^. BlockDataRefGasUsed E.>=. E.val g1 ) E.&&. (a E.^. BlockDataRefGasUsed E.<=. E.val g2)  E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
        E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
        E.limit $ limit
        E.offset $ (limit * off)
        return t
    returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}
        where 
            limit = (fromIntegral $ fetchLimit :: Int64)
            off   = (fromIntegral $ offset :: Int64)