module Handler.BlkGasRange where

import Import

import Handler.Common
import Blockchain.Data.DataDefs

import Data.Aeson
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import qualified Database.Esqueleto as E
       
import Data.List
       
import qualified Prelude as P
import Handler.JsonJuggler
       
getBlkGasRangeR :: Integer -> Integer -> Handler Value
getBlkGasRangeR g1 g2      =    do addHeader "Access-Control-Allow-Origin" "*"
                                   blks <- runDB $ E.select $
                                        E.from $ \(a, t) -> do
                                        E.where_ ( (a E.^. BlockDataRefGasUsed E.>=. E.val g1 ) E.&&. (a E.^. BlockDataRefGasUsed E.<=. E.val g2)  E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
                                        E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
                                        E.limit $ fetchLimit
                                        return t
                                   returnJson $ nub $ P.map bToBPrime (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}
