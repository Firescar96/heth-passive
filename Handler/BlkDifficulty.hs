module Handler.BlkDifficulty where

import Blockchain.Data.DataDefs
import Data.List
import Database.Persist.Postgresql
import Handler.Common
import Handler.JsonJuggler
import Import
import qualified Database.Esqueleto as E
import qualified Prelude as P

getBlkDifficultyR :: Integer -> Handler Value
getBlkDifficultyR d      =      do
    addHeader "Access-Control-Allow-Origin" "*"
    blks <- runDB $ E.select $
        E.from $ \(a, t) -> do
            E.where_ ( (a E.^. BlockDataRefDifficulty E.==. E.val d ) E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
            E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
            E.limit $ fetchLimit
            return t
    returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}
