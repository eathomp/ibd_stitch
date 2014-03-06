{-# LANGUAGE TupleSections #-}
module GenoEquiv (uniformEquivMat,allEquivMats,uniformList,linearCombMats) where

import Common

import Prelude hiding (replicate, map, foldl1, zipWith, map, concat, reverse, length, head, map, concatMap, (++), zipWith)
import Data.List.Stream as DL
import Numeric.Container as NC
import Data.Function (on)
import Control.Monad (ap)

uniformList :: Int -> [Double]
uniformList n = replicate n (1 / fromIntegral n)

uniformEquivMat n = linearCombMats (allEquivMats n) (uniformList n)

allEquivMats n = let states = allStates n
                in map (permMat states) (allBoolLists (div n 2))

linearCombMats :: [Matrix Double] -> [Double] -> Matrix Double
linearCombMats mats coefs = foldl1 add $ zipWith scale coefs mats

permMat states bs = fromRows $ map (destIndicator states bs) states

standardBasisVec :: Int -> Int -> Vector Double
standardBasisVec n i = let coord = [(i,1.0)]
                         in assoc n 0 coord


--destIndicator :: [IbdState] -> [Bool] -> IbdState -> NC.Vector Double
destIndicator states bs xs = let flipped = reCode  ( concat ( flipPairs bs (pairOff xs) reverse))
                                 coord = (,1.0)
                                 n = length states 
                                 in head $ map (standardBasisVec n) $ DL.elemIndices flipped states
                                     
allBoolLists 0 = [[]]
allBoolLists n = concatMap extend (allBoolLists (n-1))
    where extend xs = map((xs++).(:[])) [True,False]

flipPairs :: [Bool] -> [a] -> (a -> a) -> [a]
flipPairs bs xs f = if (length bs `mod` 2) == 0 
                        then zipWith g bs xs
                        else error "Not an even list"
    where g b x = if b then f x else x


