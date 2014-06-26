{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}    
module Types where

import Data.Monoid
import Data.IntSet (IntSet, member)
import Data.Bits ((.&.), bit, (.|.), testBit)
import Control.Lens.Wrapped
import Control.Lens.Indexed
import Control.Lens.Iso
import Control.Lens 
import Control.DeepSeq
import Control.Monad.Reader
import Numeric.Container hiding (join)
import Data.Map (Map)
import qualified Data.Bimap as BMap
import Data.Word
import Data.List.Stream (foldl')
import Control.Monad.Random
import System.Random.TF
import Control.Applicative
import Math.Polynomial (Poly)
import MyPart as MP
import qualified Data.Vector as DV

magicFifteen = 15

type Dist = Vector Double
type PhaseDist = [Double]

data MatPat = Mat | Pat deriving (Eq, Show, Ord)

matPat :: String -> (HapId, HapId)
matPat x = over both (x,) (Mat, Pat)

type HapId = (String, MatPat)

data PosDifference = Diff Double | Start deriving Show


data Marker = Marker {tMat :: Matrix Double
                    , emitMap :: Map String (Vector Double)} deriving Show



data Indiv = Indiv {name::String
                  , genos::DV.Vector String 
                  , phasInd::Maybe [Int]
                  , _intIds::(Int, Int) } deriving Show
makeLenses ''Indiv

data PhaseMode = Phased | Unphased (Matrix Double) deriving Show

type PhaseInput = (String, [Int]) 

data ModelNode = ModelNode Marker String (Maybe Constraint)

data ModelInfo = ModelInfo { states :: [IbdState]
                           , nStates :: Int
                           , changeRateMI :: Double 
                           , phaseMode :: PhaseMode
                           , markers :: DV.Vector Marker
                           , markerPos :: DV.Vector Double
                           , priorMarkers :: Maybe IntSet
                           , marginal :: Dist 
                           , prior :: Maybe IbdResults
                           , hapIntMap :: BMap.Bimap HapId Int} deriving Show

newtype Constraint = Constraint {getConstraint :: Word} deriving (Eq, NFData)
instance Monoid Constraint where
        mempty = toMask $ replicate magicFifteen True
        mappend (Constraint x)  (Constraint y) = Constraint $  x .&. y
instance Show Constraint where 
        show x@(Constraint y)= show . (,) y . zip [0..] $ fromMask x

toMask :: [Bool] -> Constraint
toMask = Constraint . foldl' (.|.) 0 . map (bit . fst) . filter snd . zip [0..]

fromMask :: Constraint -> [Bool] 
fromMask (Constraint x) = Prelude.map (testBit x) . take magicFifteen $ [0..]


type ModelR = Reader ModelInfo
type ModelSimR = RandT TFGen (Reader ModelInfo)

type FglList = [(Int, Int)] -- (fgl, location)
type StateList = [(Int, Int)]
type IbdResults = DV.Vector (MP.IntPartition)
--type IbdResults' = (IbdResults, DV.Vector (DP.Partition HapId))
type IbdState = [Int]

type ConsVec = DV.Vector (Constraint)

data Config = Config { markerFile :: FilePath
                     , phasingFile :: Maybe FilePath
                     , priorFiles :: Maybe [FilePath]
                     , priMarks :: Maybe [Int]
                     , iterations :: Maybe Int
                     , outFile :: FilePath
                     , seed :: Int
                     , popKin :: Double
                     , changeRate :: Double
                     , nullFrac :: Double
                     , hapSetSize :: Int
                     , genoError :: Double
                     , phaseFlag :: PhaseMode} 
                       deriving Show
