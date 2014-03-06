module Poset (geq) where 
import Common
import Data.Function (on)
import Control.Monad (filterM, join)
import Data.Set (empty, insert, Set, isSubsetOf)

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

toRelation :: IbdState -> Set (Int,Int)
toRelation xs =  foldl (flip insert) empty $
        concatMap (\ys -> [(x,y) | x <- ys, y <- ys]) $ groupList xs [0..]

bob = map toRelation statesFour

phil = [isSubsetOf x y | x <- bob, y <- bob]

geq :: IbdState -> IbdState -> Bool
--geq x y = isSubsetOf (toRelation y) (toRelation x)
geq = isSubsetOf `on` toRelation 

leq :: IbdState -> IbdState -> Bool
leq = not .: geq

