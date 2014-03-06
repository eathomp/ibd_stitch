{-# LANGUAGE TupleSections #-}
module ParseMorg (getConfig, getGenoData, addPhasingTo, getFglGraphs, printMorgGraph) where

import Common
import GenoEquiv 
import Trans (margDist, makeWeirdMat, makeTrans)
import Emission (makePolyMap)

import Text.Parsec 
import Text.Parsec.String (Parser,parseFromFile) -- type Parser = Parsec String ()
import Text.Parsec.Expr
import Text.Parsec.Error
import Text.Parsec.Language (javaStyle)
import Text.Parsec.Token as P
import Control.Applicative ((<*),(<*>),(*>),pure,(<$>))
import Control.Arrow ((&&&),(***))
import qualified Data.Sequence as DS
import Data.Maybe (fromMaybe,isJust)
import Data.List.Stream (transpose, nub, foldl1', partition, intercalate)
import Data.IntSet (fromList)
import Math.Polynomial
import qualified Data.Map as Map (map) 
import qualified Data.Bimap as BMap
import qualified Numeric.Container as NC (fromList, ident)
import qualified Data.Vector as DV
import Control.Lens (set, view, toListOf, folded, both)


getGenoData ::  Config -> String -> (ModelInfo, [Indiv])
getGenoData cfg x = checkGenoData cfg $ parse parseGenoData "marker file" x

getConfig ::  String -> Config
getConfig x = checkConfig $  parse parseConfig ".par file" x

getFglGraphs ::  Int -> String -> [[(String, FglList)]]
getFglGraphs n = either (error . show) checkFglGraphs . parse (parseFglGraphs n) "FGL file"

checkFglGraphs ::  [[a]] -> [[a]]
checkFglGraphs = check (allEqual . map length) "Different numbers of indivs in realizations!"

addPhasingTo :: [Indiv] -> String -> [Indiv]
addPhasingTo inds x = let prs = parse parsePhaseData "phasing file" x 
                          f x = map (addPhasingToInd x) inds
                          in either (error . show) f prs 

addPhasingToInd ::  [PhaseInput] -> Indiv -> Indiv
addPhasingToInd ph ind = ind { phasInd = g (lookup (name ind) ph) }
          where g = maybe (error "missing phasing data") Just

checkConfig :: Either ParseError [(String,String)] -> Config
checkConfig assoc = case assoc of
    Left x -> error $ show x
    Right x -> Config { markerFile = gv "input marker data file"
                      , phasingFile = read `fmap` lookup "input extra file" x
                      , priorFiles = getOptionalValue x "priors"
                      , iterations = getOptionalValue x "iterations"
                      , priMarks = getOptionalValue x "select markers"
                      , outFile = gv "output extra file"
                      , popKin = gv "population kinship"
                      , changeRate = gv "kinship change rate"
                      , nullFrac = gv "transition matrix null fraction"
                      , seed = gv "seed"
                      , hapSetSize = 4 -- HARD CODE
                      , logFile = gv "logfile"
                      , genoError = gv "genotyping error rate"
                      , phaseFlag = getPhase hss x}
                 where gv :: (Read a) => String -> a
                       gv = getConfigValue x
                       hss = gv "haplotype set size"

getConfigValue :: (Read a) => [(String,String)] -> String -> a
getConfigValue assoc name = fromMaybe err $ lookup name assoc >>= readMaybe 
    where err = error $ "Error reading parameter '" ++ name ++ "'"

getOptionalValue :: (Read a) => [(String,String)] -> String -> Maybe a
getOptionalValue assoc name = (fromMaybe err . readMaybe) `fmap` lookup name assoc-- >>= return $ fromMaybe err readMaybe 
    where err = error $ "Error reading parameter '" ++ name ++ "'"

getPhase n assoc  
        | f "phased data" = Phased
        | f "unphased data" = Unphased $ uniformEquivMat n
        | otherwise = error "Unknown or unspecified phasing option"
        where f x = isJust $ lookup x assoc

-- config parsing
parseConfig = myWs >> endBy (selectMarkersLine <|> line) myWs <* eof
line = selLine <|> priorLine <|> regularLine 
lineBody = many1 $ noneOf "\n\r"
regularLine = let snip xs = (unwords $ init $ words xs, last $ words xs)
                  in snip <$> (optional (myRes "set") *> lineBody)
selLine = (,"") <$> (myRes "select" *> lineBody )
priorLine = (,) <$> (myRes "set priors" *> return "priors") <*> lineBody 
selectMarkersLine = (,) <$> (myRes "select markers" *> return "select markers") <*> (show `fmap` many myInt )

parseGenoData :: Parser (DV.Vector (Maybe Double, Double), DV.Vector Double, [Indiv])
parseGenoData = do 
    myRes "map marker positions" 
    pos <- many myFloat
    freq <- many alFreqLine
    myRes "set marker data" >> myInt
    theIndivs <- many (myIndiv (length pos)) <* eof
    let diffs = Nothing : fmap Just (diff pos)
        tup (x:y:_) = (x,y)
        idIndivs = zipWith (set intIds) (map tup $ pairOff [1..]) theIndivs
    return (DV.fromList $ zip diffs freq, DV.fromList pos, idIndivs)

parsePhaseData :: Parser [PhaseInput]
parsePhaseData = endBy phaseLine newline <* eof

checkGenoData :: Config -> Either ParseError (DV.Vector (Maybe Double, Double), DV.Vector Double, [Indiv]) -> (ModelInfo, [Indiv])
checkGenoData cfg x = case x of 
    Right (marks, pos, inds) -> 
        if allEqual $ DV.length marks : map (DV.length . genos) inds
        then (makeModelInfo cfg marks pos inds, inds)
        else error "Different numbers of markers and data supplied"
    Left err -> error $ show err

makeModelInfo :: Config -> DV.Vector (Maybe Double, Double) -> DV.Vector Double -> [Indiv] -> ModelInfo 
makeModelInfo cfg marks pos inds = let pk = popKin cfg
                                       hss = hapSetSize cfg
                                       sts = allStates hss
                                       marg = margDist pk sts
                                       pmap = makePolyMap (genoError cfg) hss
                                       (tm, rv) = makeWeirdMat hss (nullFrac cfg) pk marg
                                       mkEmits p = Map.map (NC.fromList . map (`evalPoly` p)) pmap
                                       mkTrans Nothing = NC.ident (length sts)
                                       mkTrans (Just d) = makeTrans tm rv (changeRate cfg) d
        in ModelInfo { states = sts
                     , nStates = length sts
                     , changeRateMI = changeRate cfg
                     , priorMarkers = fmap (fromList . map (+ (-1))) (priMarks cfg) -- the program is zero-indexed but input is one-indexed!
                     , phaseMode = phaseFlag cfg 
                     , markers = DV.map (uncurry Marker . (mkTrans *** mkEmits)) $ marks
                     , markerPos = pos
                     , marginal = marg
                     , prior = Nothing
                     , hapIntMap = makeHapIntMap inds }

printMorgGraph :: [(String, FglList)] -> String
printMorgGraph = intercalate "\n" . map ( \ (nm, xs) -> unwords $ nm : "0" : (show . fst . head) xs : show (length xs - 1) : (tail . init . map show . concatMap detup) xs)
    where detup (x, y) = [x, y]


parseFglGraphs :: Int -> Parser [[(String, FglList)]]
parseFglGraphs n = do
        graphs <- endBy (oscLine n) newline <* eof
        let k = (2*) . length . nub . map fst $ graphs 
        return $ if length graphs `mod` k == 0 
                     then chunk k graphs 
                     else error $ "Incomplete hap set entered? Should be size " ++ show (length graphs `div` k)

makeHapIntMap :: [Indiv] -> BMap.Bimap HapId Int 
makeHapIntMap xs = BMap.fromList $ (zip ) (go (matPat . name)) (go (view intIds) )
        where go f = toListOf (folded.both) $ map f xs

oscLine n = do
        name <- myWord
        mySpace; myDec; mySpace
        first <- fmap fromInteger myDec
        mySpace
        nswitch <- myDec
        foo <- count (2 * fromInteger nswitch) (mySpace *> fmap fromInteger myDec)
        optional mySpace
        let tuple_pairs = uncurry zip . (head &&& last) . transpose . pairOff
            cap = (\(x, y) -> x ++ [(fst $ head y, n)]) . partition ((< n) . snd)
        return (name, cap . tuple_pairs $ first:foo ++ [n])

-- marker parsing
myWord = many1 ( noneOf " \t\n\r") <* myWs <?> "word"
myIndiv n = Indiv <$>  myWord <*> (DV.fromList <$> count n myGeno) <*> pure Nothing <*> pure (-999999,-999999) <?> "indiv"
myAllele = oneOf "012" <* myWs  <?> "allele"
myGeno = listify <$> myAllele <*> myAllele <?> "geno"
    where listify x y = [x,y]
alFreqLine = try (myRes "set marker" >> myInt) >> count 2 myWord >>
    myFloat <* myFloat <?> "allele freq"

phaseLine = (,) <$> myWord <* myWord <*> sepBy  myPhase (char ' ') 
myPhase = read <$> (string "0" <|> string "1" <|> string "-1")
mySpace = many $ oneOf "\t " -- no newline


-- parsing setup
def = javaStyle {reservedNames = ["select"
                                 , "map marker positions"
                                 , "set marker"
                                 , "set marker data"
                                 , "set priors"]
                , commentLine = "#" }
TokenParser {reserved = myRes
            , decimal = myDec 
            , whiteSpace = myWs 
            , float = myFloat
            , integer = myInt}  = makeTokenParser def
-- helpers
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

diff = zipWith (-) =<< tail


