module Trans (makeTransMat, makeTrans, makeWeirdMat, margDist, transMat, normalize, reWeight) where

import Common
import Control.Monad.Reader
import Data.List.Stream (nub,elemIndices,sortBy)
import Numeric.LinearAlgebra
import Control.Applicative
import Data.Function (on)

makeTransMat :: ModelNode -> ModelR (Matrix Double)
makeTransMat (ModelNode mark _ _) = do
         phm <- asks phaseMode 
         let f Phased = return $ tMat mark
             f (Unphased _) = f Phased
             in f phm

makeTrans :: Matrix Double -> Vector Double -> Double -> Double -> Matrix Double
makeTrans tm rv cr d = let scaleVec = mapVector (\ x -> exp ((-d) * cr * x)) rv
                           in diag scaleVec + diag (mapVector (1-) scaleVec) <> tm

{-
- The new idea here is that at each locus, the transition vector at each row is
- a mixture of staying probability and jumping probability.  Jumping is
- itself a mixture of allowed and marginal transitions.  The rate vector
- must be kept around to calculate staying probability.
-}
makeWeirdMat :: Int -> Double -> Double -> Dist -> (Matrix Double, Vector Double)
makeWeirdMat n t popkin marg = ( scale (1-t) jumpMat + scale t nullMat, (-1) * takeDiag qMat)
    where jumpMat = diag (mapVector ((-1)/) $ takeDiag qMat) <> (qMat - diag diagQ)
          qMat = transMat popkin (allStates n)
          nullMat = fromRows $ replicate (dim marg) (reWeight diagQ marg)
          diagQ = takeDiag qMat


transMat ::  Double -> [IbdState] -> Matrix Double
transMat beta states = ratesMat - diag (rowSums ratesMat)
    where ratesMat = fromRows $ fmap (fromList . outRates beta states) states
          rowSums = fromList . map sumElements . toRows

tabulate xs = (ys , map (count xs) ys)
    where ys = nub xs

margDist :: Double -> [IbdState] -> Dist
margDist = (fromList .) . map . margProb

{- Ewens sampling formula modified to be a function of IBD state, not class count.
   This requries dividing the ESF value by the number of states sharing
   the class count.  
-}

margProb ::  Eq a => Double -> [a] -> Double
margProb beta state = exp $ logK + loggo + bing
    where (classes,counts) = tabulate state
          copyCounts = map fromIntegral $ take (length state) $ map (count counts) [1..]
          logK = negate $ sum $ map (log . (+1) . (*beta) . fromIntegral) [1..length state - 2]
          loggo = ((log beta*) . fromIntegral) (length state - length classes) + 
                      ((log (1 - beta)*) . fromIntegral) (length classes - 1)
          bing = sum $ zipWith (*) copyCounts (map (log . fac) [0..])

fac :: Int -> Double
fac n =  fromIntegral $ product [1..n]

count xs x = length $ filter (==x) xs


listUpdate xs i new = (\(l,_:r) -> l ++ new:r) $ splitAt i xs

-- all jumps to other states where destination state is one of (destF fromState)
getDests destF fromState = filter (fromState/=) $ 
 ( (reCode .) . listUpdate fromState) <$> [0..length $ tail fromState] <*> destF fromState

{-    concat [let n =  length $ filter (==new) fromState in 
                map reCode (listUpdate fromState i new n) 
                | i <- [0..length fromState - 1], new <- destF fromState] -}

-- countDests st dl df gives count of times each state in dl is a dest of st with fun f 
countDests :: IbdState -> [IbdState] -> ([Int] -> [Int]) -> [Int]
countDests fromState destList destF = fmap f destList
    where f = length . (`elemIndices` getDests destF fromState)

-- rates to existing labels are beta, to new label are 1-beta
outRates :: Double -> [IbdState] -> IbdState -> [Double]
outRates beta dests fromState = zipWith f existingDests newDests
    where existingDests =  cfd id
          newDests = cfd $ return . (+1) . length
          cfd = countDests fromState dests
          f x y =  beta * fromIntegral x + (1-beta) * fromIntegral y


-- Utilities

normalize :: Vector Double -> Dist
normalize v = (1 / sumElements v) `scale` v

reWeight :: Vector Double -> Vector Double -> Dist
reWeight = normalize .: mul




--
--    http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hmatrix
--Data.Vector
--Data.Packed.Vector
--Numeric.Container
--Numeric.LinearAlgebra.Util
--transpose [[]]!
