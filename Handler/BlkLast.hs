module Handler.BlkLast where
    
import Blockchain.Data.DataDefs
import Data.List
import Handler.Common 
import Handler.JsonJuggler
import Import
import qualified Database.Esqueleto as E
import qualified Prelude as P
         

getBlkLastR :: Integer -> Handler Value
getBlkLastR n = do 
    addHeader "Access-Control-Allow-Origin" "*"
    blks <- runDB $ E.select $
        E.from $ \(a, t) -> do
        E.where_ (  a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)
        E.limit $ P.min (fromIntegral n :: Int64) fetchLimit 
        E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
        return t
    returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}
                                
