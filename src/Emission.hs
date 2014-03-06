module Emission (makeEmissionVec, makePolyMap) where

import Prelude hiding (map, elem, product, foldr, zip, (++), length, filter, (!!), iterate, concatMap, all, sum)
import Data.Function (on)
import Data.List.Stream 
import Data.Monoid (getAll, All)
import Numeric.Container
import GenoEquiv
import Control.Arrow ((&&&))
import Math.Polynomial
import Data.Bits (testBit)

import Common
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

makeEmissionVec :: ModelNode -> ModelR (Vector Double)
makeEmissionVec (ModelNode mark alleleData mc) = do
        sts <- asks states
        n <- asks nStates
        phm <- asks phaseMode
        let the_map = emitMap mark
        let ep = if '0' `elem` alleleData
                      then constant 1.0 n
                      else fromMaybe (error $ "Bad data!: " ++ alleleData) $ Map.lookup alleleData the_map
        let rawVec = case phm of
                Phased -> ep
                Unphased mat -> mat <> ep
        return $ maybe rawVec (mul rawVec . deBool) mc

-- | Creates a lookup map for SNP genotype strings whose entries are lists
-- of Poly, corresponding to an emission vector (takes an error rate and 
-- length of the data vectors )
makePolyMap :: Double -> Int -> Map.Map String [Poly Double]
makePolyMap e n = Map.fromList $ map poly_list (allAlleleData n)
        where poly_list str = (str, map (getPoly e str) (allStates n))

-- | Poly of a string and an IBD state given error rate, as a function of allele frequency
getPoly :: Double -> String -> IbdState -> Poly Double
getPoly e str state = foldl1' multPoly $ collapseOnLabel (classPoly e) state str

-- | Poly for a string given error rate, as a function of allele frequency
classPoly :: Double -> String -> Poly Double
classPoly e xs = scalePoly (pr (1-e)) x `addPoly` scalePoly (pr e) (poly LE [1, -1])
        where pr p = (p ^ k) * ((1 - p) ^ (n - k))
              n = length xs
              k = (length . filter (=='1'))  xs


-- group values into lists by label, then apply a function to each list
collapseOnLabel :: Ord k => ([a] -> b) -> [k] -> [a] -> [b]
collapseOnLabel f labels values = map f (Map.elems $ groupOnLabel labels values)

-- given e error and p0 prob the true class allele is 0, prob of a string of 1's and 2's
classProb :: Double -> Double -> String -> Double
classProb e p0 xs = p0 * pr (1-e) + (1-p0) * pr e
    where pr p = (p ^ k) * ((1 - p) ^ (n - k))
          n = length xs
          k = (length . filter (=='1'))  xs

allAlleleData n = (!!(n-1)) $ iterate (concatMap f) ["1","2"]
    where f xs = map (:xs) "12"

deBool :: Constraint -> Vector Double
deBool (Constraint x) = buildVector magicFifteen (\ i -> if testBit x i then 1 else 0)



