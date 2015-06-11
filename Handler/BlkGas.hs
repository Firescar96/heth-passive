module Handler.BlkGas where

import Blockchain.Data.DataDefs
import Data.List
import Database.Persist.Postgresql
import Handler.Common
import Handler.JsonJuggler
import Import
import qualified Database.Esqueleto as E
import qualified Prelude as P

getBlkGasR :: Integer -> Handler Value
getBlkGasR g = do
    addHeader "Access-Control-Allow-Origin" "*"
    blks <- runDB $ E.select $
        E.from $ \(a, t) -> do
        E.where_ ( (a E.^. BlockDataRefGasUsed E.==. E.val g)  E.&&.( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
        E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
        E.limit $ fetchLimit
        return t
    returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}
