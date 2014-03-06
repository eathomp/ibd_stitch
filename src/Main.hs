{-# LANGUAGE OverloadedStrings #-}
import Common
import ParseMorg
import Trans
import Emission
import Calcs
import Joint
import GenoEquiv
import Run
import Fgl


import System.Environment
import System.IO
import Control.Parallel.Strategies
import System.Random
import Data.Time
import GHC.Conc (numCapabilities)
import Control.Monad (join)
import Data.Traversable (traverse)
import qualified Data.Vector as DV
import Debug.Trace
import Text.Show.Pretty  (ppShow)

main = do 
    print numCapabilities
    parFileName <- fmap cmdLineCheck getArgs
    config <- getConfig `fmap` readFile parFileName
    (mi1, theIndivs1) <- getGenoData config `fmap` readFile (markerFile config)
    let yak = iterate (snd . split) $ mkStdGen (seed config)
    theIndivs <- maybe (return theIndivs1) (fmap (addPhasingTo theIndivs1) . readFile) (phasingFile config)
    let nmarks = DV.length . genos . head $ theIndivs
    infos <- getModelInfos nmarks config mi1
    let results = withStrategy (parList rpar) $ zipWith3 ( fglGraph theIndivs ) infos yak [1..]
    putStrLn $ "Doing " ++ show (length infos) ++ " realizations"
    currTime <- getCurrentTime
    appendFile (logFile config) "IBD Stitch\n"
    appendFile (logFile config) ("Config for run at " ++ show currTime ++ ":\n" ++ ppShow (config{priMarks = Just []}))
    withFile (outFile config) WriteMode $ \the_file -> do
            mapM (hPutStrLn the_file . printMorgGraph) results

cmdLineCheck x = case x of
                     [] -> error "No .par file specified on command line."
                     (x:xs) -> x

doParListIO ::  IO [a] -> IO [a]
doParListIO = fmap $ withStrategy (parListChunk 1 rpar)

doParList ::  [a] -> [a]
doParList = withStrategy (parList rpar)



