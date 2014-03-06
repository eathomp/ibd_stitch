{-# LANGUAGE TupleSections, TypeSynonymInstances #-}
module Fgl (indsToFglLists, fglGraphToParts) where 
-- note resultsToPartitions may be unneeded
import Common
import Debug.Trace --- kil kil kil
import Control.Arrow 
import Control.Applicative
import Data.Monoid
import Data.Traversable (traverse)
import Data.List.Stream (elemIndex, group, groupBy, nub, intercalate, mapAccumL, tails, transpose)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as Map
import Data.Foldable (foldl', foldr', foldMap)
import Data.Array.IArray
import Data.Tuple
import qualified Data.Vector as DV
import Data.Function
import MyPart as MP
import Control.Monad.State hiding (join)
import Control.Lens (both, toListOf , over, folded, view)
import qualified Data.Bimap as BMap

indsToFglLists :: IbdResults -> [Indiv] -> [(String, FglList)]
indsToFglLists parts_list inds = map bork hob
    where bork = second $ \ x -> fglEncode .  DV.toList $ DV.map (flip rep x) parts_list
          hob = toListOf (folded . both) . map hug $ inds
          hug i = over both (name i,) (view intIds i)
          
fglGraphToParts :: BMap.Bimap HapId Int -> [(String, FglList)] -> IbdResults
fglGraphToParts bm xs = DV.map (foldr add_group MP.empty . Map.elems . flip groupOnLabel hap_nums) . DV.fromList . transpose $ map (fglDecode . snd) xs
    where add_group (y:ys) p = foldr (join y) p ys
          hap_nums = catMaybes $ map (`BMap.lookup` bm) $ zip (fst $ unzip xs) (cycle [Mat, Pat]) -- recall there are two FGL lists per input indiv
          -- ^^ use catMaybes to basically do a left join and throw out FGL info on individuals we don't have markers for

-- takes a pair of indivs, gives an array of which haplotype pairs are IBD
-- in the various IBD state
pairsArray :: (String, String) -> Array Int [(HapId, HapId)]
pairsArray names = listArray (0, length statesFour - 1) $ map equiv_pairs statesFour
        where equiv_pairs st = concatMap allPairs . groupList st .toListOf (both . both) . over both matPat $ names


cumSum = scanl1 (+) 

detup (x, y) = [x, y]

fglEncode ::  [Int] -> FglList
fglEncode = snd . mapAccumL f 0 . group
    where f loc = (snd &&& id) . (head &&& ((+) loc . length))

fglDecode :: FglList -> [Int]
fglDecode = concat . snd . mapAccumL f 0
    where f loc (lab, end) = (end, replicate (end - loc) lab)


