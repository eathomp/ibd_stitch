{-# LANGUAGE ViewPatterns #-}
module Calcs (forwardBackward, forward, forwardBackwardSim) where 

import Common
import Trans
import Emission
import Control.Monad.Reader
import Control.Monad.Random hiding (fromList)
import Control.Monad.State.Strict
import Data.Traversable (traverse)
import Numeric.LinearAlgebra hiding ((|>))
import Data.Function (on)
import Data.List.Stream (findIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as DV

forwardBackward :: ModelInfo -> Dist -> DV.Vector ModelNode -> DV.Vector Dist
forwardBackward info init theData = flip runReader info $ do
    forwardProbs <- evalStateT (traverse forward theData) init 
    ones <- constant 1 `fmap` asks (length . states) 
    backwardProbs <- evalStateT (traverse backward theData) ones 
    return . DV.tail $ DV.zipWith reWeight (DV.cons ones forwardProbs) (DV.snoc backwardProbs ones)

forwardBackwardSim :: DV.Vector ModelNode -> ModelSimR (DV.Vector Int)
forwardBackwardSim the_vec = do
        init  <- asks marginal
        forwardProbs <- lift $ evalStateT (traverse forward the_vec) init 
        ones <- constant 1 `fmap` asks (length . states) 
        DV.reverse `fmap` evalStateT (traverse backwardSim (DV.reverse (DV.zip the_vec forwardProbs)) ) ones

forward :: ModelNode -> StateT Dist ModelR Dist
forward rn = do
        dist <- get
        theTMat <- lift $ makeTransMat rn
        ep <- lift $ makeEmissionVec rn
        put . normalize $ mul (trans theTMat <> dist) ep
        get

backward :: ModelNode -> StateT Dist ModelR Dist
backward rn = do
        dist <- get
        theTMat <- lift $ makeTransMat rn
        ep <- lift $ makeEmissionVec rn
        put . normalize $ theTMat <> (ep `mul` dist)
        get

backwardSim :: (ModelNode, Dist)  -> StateT Dist ModelSimR Int
backwardSim (mn, fwd) = do
        inDist <- get
        ri <- lift $ randIndex (fwd * inDist)
        outDist <- lift $ ((!!ri) . toColumns) `fmap` lift (makeTransMat mn)
        put outDist
        return ri

randIndex :: Vector Double -> ModelSimR Int
randIndex weights = do
        rand <- getRandomR (0::Double,1)
        rand `seq` return . findDouble rand . normalize $ weights

findDouble :: Double -> Vector Double -> Int
findDouble t v = go 0.0 0  
        where go t' i | new > t || i == (n-1) = i
                      | otherwise             = go new (i+1)
                      where new = t' + v@>i
              n = dim v

cumSum = scanl1 (+) 
