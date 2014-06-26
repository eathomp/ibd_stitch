{-# LANGUAGE TupleSections, ViewPatterns #-}
module Joint (fglGraph ) where 
import Common
import Run
import Calcs
import Fgl
import Poset (geq)

import System.Random 
import System.Random.TF
import System.Random.Shuffle (shuffleM)
import Control.Monad.Random
import Data.List.Zipper as DZ hiding (insert)
import Data.Array.IArray
import Prelude hiding (lookup, foldl1)
import Numeric.Container as NC hiding ((<>), find)
import Data.Tuple
import Control.Arrow ((&&&), first, second,(***))
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.List.Stream (concat, elemIndex, nub, foldl', findIndices, foldl1, transpose, findIndex)
import Data.Foldable as DF (foldMap, toList, fold)
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Bimap as BMap
import Data.Monoid 
import Data.Function 
import Control.Applicative ((<|>),(<*>),(<$>),pure)
import Control.Monad.Reader hiding (sequence)
import Control.Monad (replicateM) -- for testing only
--import Data.Set as DS (Set, member, intersection, difference, singleton, delete, partition, union)
import Data.IntSet as DS (IntSet, member, intersection, difference, singleton, delete, partition, union)
--import qualified Data.Set as DS 
import qualified Data.IntSet as DS 
import Control.Lens.Indexed (imap)
import Control.Lens (over, both, view)
import Control.Lens.Fold
import Control.DeepSeq
--import Data.Partition as DP
import MyPart as MP
import qualified Data.Vector as DV



-- what in the christ
preCons' :: IntSet ->  Indiv -> Indiv -> IbdResults -> ConsVec
preCons' alr x y = DV.map ( (doOnePreCons alr `on` (view intIds)) x y)

doOnePreCons :: IntSet -> (Int, Int) -> (Int, Int) -> IntPartition -> Constraint
doOnePreCons alr x y prev = a <> b <> c <> d 
          where (inx, iny) = over both (uncurry (equiv prev)) (x, y)
                (setx1, setx2) =  over both (intersection alr . myfind prev) x
                (sety1, sety2) =  over both (intersection alr . myfind prev) y
                a = checkIfForced setx1 sety1 consY11 consN11
                b = if not inx 
                        then outbredFirst <> checkIfForced setx2 sety1 consY21 consN21
                        else inbredFirst
                c = if not iny then outbredSecond <>  checkIfForced setx1 sety2 consY12 consN12 else inbredSecond
                d = if not $ or [inx, iny] 
                        then checkIfForced setx2 sety2 consY22 consN22
                        else mempty

postConsHomz' :: IntSet -> Indiv -> IbdResults -> ModelSimR ConsVec
postConsHomz' ny x prev = do 
                     which_markers <- fromMaybe (error "No markers specified for prior IBD") `fmap` asks priorMarkers
                     prior_ibd <- (fromJust) `fmap` asks prior
                     return $ DV.imap (\ i (p1,p2) -> if (i `member` which_markers) then doOnePostConsHomz ny x p1 p2 else mempty) $ DV.zip prior_ibd prev

doOnePostConsHomz :: IntSet -> Indiv -> IntPartition -> IntPartition -> Constraint
doOnePostConsHomz ny x pri prev = if uncurry (equiv pri) (view intIds $ x) || b 
                                                               then inbredFirst 
                                                               else if allOf both (not . null) (setsx )
                                                                        then outbredFirst
                                                                        else mempty
        where b = or $ (equiv prev) <$> x1 <*> x2
              setsx@(x1, x2) = over both (DS.toList . intersection ny . myfind pri) (view intIds $ x)

postCons' :: IntSet -> Indiv -> Indiv -> IbdResults -> ModelSimR ConsVec
postCons' ny x y prev = do 
                     which_markers <- fromMaybe (error "No markers specified for prior IBD") `fmap` asks priorMarkers
                     prior_ibd <- (fromJust) `fmap` asks prior
                     return $ DV.imap (\ i (p1,p2) -> if (i `member` which_markers) then (doOnePostCons ny `on` (view intIds)) x y p1 p2 else mempty) $ DV.zip (prior_ibd) prev

doOnePostCons :: IntSet -> (Int, Int) -> (Int, Int) -> IntPartition -> IntPartition -> Constraint
doOnePostCons ny x@(x1,x2) y@(y1,y2) pri prev = a <> b <> c <> d 
          where (inx, iny) = over both (uncurry (equiv prev)) (x, y)
                (setx1, setx2) = over both ((intersection ny) . myfind pri) x
                (sety1, sety2) = over both (myfind prev) y
                a =  if equiv pri x1 y1 || (iny && (equiv (pri) x1 y2) ) || (inx && (equiv pri x2 y1) ) then consY11 else checkIfForcedPri setx1 sety1 consY11 consN11
                b = if not inx 
                        then if equiv pri x2 y1 || ( iny && (equiv pri x2 y2) ) then consY21 else checkIfForcedPri setx2 sety1 consY21 consN21
                        else checkIfForcedPri (setx1 `union` setx2) sety1 consY21 consN21 <> checkIfForcedPri (setx1 `union` setx2) sety2 consY12 consN12
                c = if not iny 
                        then if equiv pri x1 y2 || (inx && (equiv pri x2 y2) ) then consY12 else checkIfForcedPri setx1 sety2 consY12 consN12
                        else mempty
                d = if not $ or [inx, iny] 
                        then if equiv pri x2 y2 then consY22 else checkIfForcedPri setx2 sety2 consY22 consN22
                        else mempty

samplePair :: Indiv -> IbdResults -> Zipper Indiv -> ModelSimR IbdResults
samplePair new prev_map (Zip already (curr:notyet)) = do
            let done_ids = DS.fromList $ concatMap (toListOf both . view intIds) already 
            let notdone_ids = DS.fromList $ concatMap (toListOf both . view intIds) notyet
            let pre' =  if null already 
                            then preConsHomz' curr (prev_map) 
                            else preCons' done_ids new curr (prev_map)
            post' <-  (if null already 
                           then DV.zipWith (<>) <$> postConsHomz' notdone_ids new (prev_map) 
                           else pure id) <*> postCons' notdone_ids new curr (prev_map) 
            marks <- asks markers
            prior_ibd <- asks prior
            let constraints = if isJust prior_ibd then DV.zipWith (<>) pre' post' else pre'
            path <- fmap DF.toList $ forwardBackwardSim $ makeRunSeq marks [new, curr] ( Just $ DV.map Just constraints)
            path `deepseq` return ( DV.zipWith (\ part ps -> DV.foldr (uncurry MP.join) part ( dairsArray (new, curr) DV.! ps)) (prev_map) (DV.fromList path))

sampleFirstPair :: Indiv -> IbdResults -> Indiv -> ModelSimR IbdResults
sampleFirstPair new prev_map curr = do
            marks <- asks markers
            path <- fmap DF.toList $ forwardBackwardSim $ makeRunSeq marks [new, curr] Nothing
            return (
                DV.zipWith (\ part ps -> DV.foldr (uncurry MP.join) part (dairsArray (new, curr) DV.! ps)) (prev_map) (DV.fromList path))


preConsHomz' :: Indiv -> IbdResults -> ConsVec
preConsHomz' x = DV.map (\ part -> if uncurry (equiv part) (view intIds $ x) then inbredSecond else outbredSecond)

dairsArray :: (Indiv, Indiv) ->  DV.Vector (DV.Vector (Int, Int))
dairsArray inds = DV.fromList $ map equiv_pairs statesFour
        where equiv_pairs st = DV.fromList $ concatMap zup . groupList st . toListOf (both . both) . over both (view intIds) $ inds
              zup (x:xs) = map ((,) x) xs

-- feels bad, this is just to pull stuff together at the top level
fglGraph ::  [Indiv] -> ModelInfo -> TFGen -> Int -> [(String, FglList)]
fglGraph inds mi gen i = trace ("Doing iteration " ++ show i) $ uncurry indsToFglLists . (buildJointIbd gen mi  &&& id) $ inds

buildJointIbd :: TFGen -> ModelInfo -> [Indiv] -> IbdResults
buildJointIbd g mi inds = flip runReader mi . flip evalRandT g $ do
    randInds <- DZ.fromList `fmap` shuffleM inds
    n <- asks (DV.length . markers)
    foldlzM attachIndiv (DV.replicate n MP.empty) randInds

attachIndiv :: IbdResults -> Zipper Indiv -> ModelSimR IbdResults
attachIndiv prevMap (Zip (oldIndiv:[]) (newIndiv:_)) = sampleFirstPair newIndiv prevMap oldIndiv -- first pair
attachIndiv prevMap (Zip oldIndivs (newIndiv:_)) = do 
    randInds <- DZ.fromList `fmap` shuffleM oldIndivs
    foldlzM (samplePair newIndiv) prevMap randInds

negateConstraint :: Constraint -> Constraint
negateConstraint = toMask . map not . fromMask

equiv :: IntPartition -> Int -> Int -> Bool
equiv p x y = rep p x == rep p y


inbredFirst :: Constraint
inbredFirst = toMask $ map (\ x -> (x !! 0 == x !! 1)) statesFour

outbredFirst :: Constraint
outbredFirst = negateConstraint inbredFirst

inbredSecond :: Constraint
inbredSecond = toMask $ map (\ x -> (x !! 2 == x !! 3)) statesFour

outbredSecond :: Constraint
outbredSecond = negateConstraint inbredSecond


consY11 :: Constraint
consY11 = toMask $ map (\ x -> (x !! 0 == x !! 2)) statesFour

consN11 :: Constraint
consN11 = negateConstraint consY11 

consY12 :: Constraint
consY12 = toMask $ map (\ x -> (x !! 0 == x !! 3)) statesFour

consN12 :: Constraint
consN12 = negateConstraint consY12 

consY21 :: Constraint
consY21 = toMask $ map (\ x -> (x !! 1 == x !! 2)) statesFour

consN21 :: Constraint
consN21 = negateConstraint consY21 

consY22 :: Constraint
consY22 = toMask $ map (\ x -> (x !! 1 == x !! 3)) statesFour

consN22 :: Constraint
consN22 = negateConstraint consY22 

interDiff :: IntSet -> IntSet -> (IntSet, IntSet)
interDiff x y = let it = intersection x y in (it, difference (union x y) it )


-- given sets to which x and y are IBD, are they forced to be IBD, not IBD,
-- or neither?
checkIfForced :: IntSet -> IntSet -> Constraint -> Constraint -> Constraint
checkIfForced x y a b  | not (DS.null int) = a
                       | not (DS.null diff) = b
                       | otherwise = mempty
                    where (int, diff) = interDiff x y 

-- same, except here x is from PRIOR ibd, so don't care if (not null $ difference y x)
checkIfForcedPri :: IntSet -> IntSet -> Constraint -> Constraint -> Constraint
checkIfForcedPri x y a b  | not . DS.null $ DS.intersection x y = a
                          | not . DS.null $ DS.difference x y = b
                          | otherwise = mempty

myfind p x = x `DS.delete`  find p x

-- my home-rolled strict zipper Monad fold, based on foldM and foldrz.
-- this could be a traversal?
foldlzM :: (Monad m) => (a -> Zipper b -> m a) -> a -> Zipper b -> m a
foldlzM f x z = if endp z then return x else do
     y <- f x z
     y `seq` foldlzM f y (right z)

