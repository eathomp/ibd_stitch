module Run where 

import Common
import ParseMorg
import Trans
import Emission
import Calcs
import Fgl

import Data.List.Stream as DL (zipWith4, zipWith3, zip3, transpose, intercalate)
import Data.Sequence as DS hiding (zipWith, replicate, take, reverse,length) 
import qualified Data.Sequence as DS (zipWith, take,length)
import Data.Traversable as DT (sequence)
import Numeric.LinearAlgebra hiding ((|>))
import Text.Printf
import qualified Data.Foldable as DF
import Control.Monad.Reader hiding (sequence)
import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Vector as DV
import qualified Data.Bimap as BMap

import Debug.Trace

getModelInfos :: Int -> Config -> ModelInfo -> IO [ModelInfo]
getModelInfos n cnf mi = fmap (map attach) $ fromMaybe err $ prior_ibd <|> pop_ibd 
        where pop_ibd = (return . flip replicate Nothing) `fmap` iterations cnf
              prior_ibd = do 
                             files <- mapM readFile `fmap` priorFiles cnf
                             return . chk $ (map Just . manglePriors n (hapIntMap mi)) `fmap` files 
              err = error "Must specify either number of iterations or prior files"
              chk =  case priorMarkers mi of
                         Just _ -> id 
                         Nothing -> error "Specified prior IBD files but not which markers to apply them at" 
              attach x = mi { prior = x }

manglePriors :: Int -> BMap.Bimap HapId Int -> [String] -> [IbdResults]
manglePriors n bm = map (fglGraphToParts bm . concat) . c1 . transpose . map (getFglGraphs n)
        where c1 = check (allEqual . map length) "Different numbers of realizations in each prior file!"


header :: Int -> [String] -> String -> String
header n names bs = f "Markers = %d\n" n . inms . nms . (++)  "\n" $ bs
    where inms = (++)  " Individual names: " 
          nms = (++) (unwords names)
          f fmt = (++) .  printf fmt

vecSeqToByteString :: [Int] -> [Double] -> DV.Vector (Vector Double) -> String
vecSeqToByteString indices positions sv = flip (++) "\n" $
        intercalate "\n" .  DL.zipWith3 formatOutputVec indices positions . DF.toList $ sv

formatOutputVec :: Int -> Double -> Vector Double -> String
formatOutputVec n pos = f "%5d" n . f "%12.6f   " pos . intercalate "  " . map (printf "%.4f") . toList
                            where f fmt = (++) .  printf fmt

makeRunSeq :: DV.Vector Marker -> [Indiv] -> Maybe (DV.Vector (Maybe Constraint)) -> DV.Vector ModelNode
makeRunSeq marks inds mcl = DV.zipWith3 ModelNode (marks) (makeAlleleData inds) constraints
    where constraints = maybe (DV.replicate n Nothing) (id) mcl
          n = DV.length marks
--check (and . map or) "Zero constraints!  Math has failed me"  . take 11000 .
badOnes = map (check or =<< show) --duh don't call this on an infinite list

makeAlleleData ::  [Indiv] -> DV.Vector String
makeAlleleData xs = foldl1 (DV.zipWith (++)) (map genos xs)
