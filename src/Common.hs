{-# LANGUAGE TupleSections #-}
module Common (module Common, module Types, module Debug.Trace) where

import Types

import Data.List.Stream (mapAccumL, nub, elemIndex, elemIndices, sortBy, groupBy, tails, group)
import Control.Monad.Random
import Control.Monad (join)
import Control.Arrow ((***),(&&&))
import Data.Function (on)
--import Data.Partition (Partition, rep)
import Data.Map as Map (Map, empty, insertWith)
import Debug.Trace
import Text.Printf
import Data.Maybe (fromMaybe)
import Data.Bits (complement)
import MyPart (IntPartition, rep)


dupMap :: (a -> b) -> (a, a) -> (b, b)
dupMap = join (***)

pairOff = chunk 2
chunk n = takeWhile (not . null) . map (take n) . iterate (drop n)

-- codings for set partitions of a set of n elements
allStates :: Int -> [[Int]]
allStates 1 = [[1]]
allStates n = concatMap extend (allStates (n-1))
    where extend xs =  map ((xs++) . (:[])) (take (maximum xs + 1) [1..])

statesFour = allStates 4

allEqual xs = and $ zipWith (==) xs (tail xs)

-- turns an arbitrary list of Ints to a canonical labelling
reCode :: [Int] -> IbdState
reCode xs = snd $ mapAccumL f ([],0) xs
    where f (dict,acc) k = case lookup k dict of
              Just lab -> ((dict,acc),lab)
              Nothing -> (((k,lab'):dict,lab'),lab')
                  where lab' = acc + 1


(.:) = (.).(.)

-- does same thing as groupOnLabel slower
groupList :: (Eq k, Ord k) => [k] -> [a] -> [[a]]
groupList labels values = getValues $ sortBy (compare `on` fst) (zip labels values)
    where getValues = map (map snd) . groupBy ((==) `on` fst)   

-- given a list of labels and some values, group the values in a map
-- indexed by the labels
groupOnLabel :: Ord k => [k] -> [a] -> Map.Map k [a]
groupOnLabel labels values = foldr ins Map.empty $ zip labels values
    where ins (k,a) = Map.insertWith (++) k [a] 

allPairs xs = join $ zipWith (map . (,)) xs (tail $ tails xs)

-- invariant: inv . inv = id
invertState :: IbdState -> IbdState
invertState = reCode . concat . reverse . pairOff


check :: (a -> Bool) -> String -> a -> a
check p str x = if p x then x else error str

if' p x y = if p then x else y

thenElseIf x y p = if p then x else y



rlEncode :: [Int] -> [(Int, Int)]
rlEncode = {-# SCC "doob" #-} map (length &&& head) . mygr

mygr :: Eq a => [a] -> [[a]]
mygr []           =  []
mygr (x:xs)       = {-# SCC "rik" #-} ys `seq` (x:ys) : mygr zs
                           where (ys,zs) = {-# SCC "fab" #-} span (== x) xs

rlDecode :: [(Int, Int)] -> [Int] 
rlDecode = concatMap (uncurry replicate)

rlMap :: (Int -> a) -> [(Int, Int)] -> [a]
rlMap _ [] = []
rlMap f ((a, b):xs) = stuff ++ rest
        where stuff = replicate a (f b)
              rest = stuff `seq` rlMap f xs

rlGet :: Int -> [(Int, Int)] -> Int
rlGet k = (!! k) . rlDecode

iterateM :: Monad m => (a -> m a) -> m a -> m [a]
iterateM f x = do
        y <- x >>= f
        ys <- iterateM f (return y)
        return $ y:ys

iterateMN :: Monad m => Int -> (a -> m a) -> m a -> m [a]
iterateMN n f x = do
        y <- x >>= f
        ys <- if n == 1 then return [] else iterateMN (n-1) f (return y)
        return $ y:ys


--debuggage
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x

traceMap :: (Show b) => (a -> b) -> String -> a -> a
traceMap f mes = trace =<< ((mes ++) . show . f)

