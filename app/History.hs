{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
  main,
) where

import Data.ByteString.Char8 qualified as B
import Data.Foldable (for_)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Time (Day, UTCTime (..), addDays, getCurrentTime)
import Distribution.Client.Config (loadConfig, savedGlobalFlags)
import Distribution.Client.GlobalFlags (globalCacheDir)
import Distribution.Simple.Flag (fromFlag)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Hackage.RevDeps (extractDependencies, latestReleases)
import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, showDefault, strArgument, value)
import Options.Applicative.NonEmpty (some1)
import System.Console.ANSI (hSupportsANSI, hyperlinkCode)
import System.FilePath ((</>))
import System.IO (stdout)
import Granite (lineGraph, defPlot)
import Data.Bifunctor (second)

data Config = Config
  { cnfStart :: !Day
  , cnfFinish :: !Day
  , cnfStep :: !Word
  , cnfPackageNames :: !(NonEmpty PackageName)
  }

parseArgs :: Day -> Parser Config
parseArgs today = do
  cnfStart <-
    option auto $
      long "start"
        <> help "Start date, YYYY-MM-DD"
        <> value (read "2006-09-01")
        <> showDefault
  cnfFinish <-
    option auto $
      long "finish"
        <> help "Finish date, YYYY-MM-DD"
        <> value today
        <> showDefault
  cnfStep <-
    option auto $
      long "step"
        <> help "Step in days"
        <> value 1000
        <> showDefault
  cnfPackageNames <-
    some1 $
      strArgument $
        metavar "PKGS"
          <> help "Package names to scan Hackage for their reverse dependencies"
  pure Config {..}

main :: IO ()
main = do
  let desc = "Count Hackage reverse dependencies for given dates, using local package index. Consider running 'cabal update' beforehand."
  today <- utctDay <$> getCurrentTime
  Config {..} <-
    execParser $
      info (helper <*> parseArgs today) (fullDesc <> progDesc desc)
  let args = NE.toList cnfPackageNames
      dates = L.nub $ [cnfStart, addDays (fromIntegral cnfStep) cnfStart .. cnfFinish] ++ [cnfFinish]
  supportsAnsi <- hSupportsANSI stdout

  cnf <- loadConfig minBound mempty
  let cacheDir = fromFlag $ globalCacheDir $ savedGlobalFlags cnf
      idx = cacheDir </> hackageHaskellOrg </> "01-index.tar"
  putStrLn $ unwords $ "Date      " : map (showPackage supportsAnsi) args
  let needles = map (B.pack . unPackageName) args
  results <- flip traverse dates $ \date -> do
    releases <- latestReleases needles idx (Just $ UTCTime date 0)
    let pkgs = fmap (extractDependencies args) releases
        pkgs' = M.mapWithKey M.delete pkgs
        allCounters :: M.Map PackageName Int
        allCounters = M.unionsWith (+) $ fmap (fmap (const (1 :: Int))) pkgs'
        counters = map (\pkg -> M.findWithDefault 0 pkg allCounters) args
    putStrLn $ unwords $ show date : zipWith (curry showPair) args counters
    pure (date, counters)
  let graphLines = zip (map unPackageName args) (uncurry (\ds -> map (zip (map (fromIntegral . fromEnum) ds) . map fromIntegral)) $ second L.transpose $ unzip results)
      graph = lineGraph "" graphLines defPlot
  putStrLn graph

showPair :: Show v => (PackageName, v) -> String
showPair (k, v) =
  replicate (length (unPackageName k) - length (show v)) ' '
    ++ show v

showPackage :: Bool -> PackageName -> String
showPackage supportsAnsi p =
  if supportsAnsi
    then hyperlinkCode ("https://" ++ hackageHaskellOrg ++ "/package/" ++ xs) xs
    else xs
  where
    xs = unPackageName p

hackageHaskellOrg :: FilePath
hackageHaskellOrg = "hackage.haskell.org"
