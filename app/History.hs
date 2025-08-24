{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
  main,
) where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (Day, UTCTime (..), addDays, getCurrentTime)
import Distribution.Client.Config (loadConfig, savedGlobalFlags)
import Distribution.Client.GlobalFlags (globalCacheDir)
import Distribution.Simple.Flag (fromFlag)
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Hackage.RevDeps (getReverseDependencies, getTransitiveReverseDependencies)
import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, showDefault, strArgument, switch, value)
import Options.Applicative.NonEmpty (some1)
import System.Console.ANSI (hSupportsANSI, hyperlinkCode)
import System.FilePath ((</>))
import System.IO (stdout)
import Granite (lineGraph, defPlot, widthChars, yBounds, xFormatter, yFormatter)
import Data.Bifunctor (second)
import System.Console.Terminal.Size qualified as TermSize

data Config = Config
  { cnfStart :: !Day
  , cnfFinish :: !Day
  , cnfStep :: !Word
  , cnfPackageNames :: !(NonEmpty PackageName)
  , cnfTransitive :: !Bool
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
  cnfTransitive <-
    switch $
      long "transitive"
        <> help "Count transitive (both direct and indirect) dependencies. This mode is imprecise when passing multiple package names at once."
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
  results <- flip traverse dates $ \date -> do
    let utcTime = Just $ UTCTime date 0
        func = if cnfTransitive then getTransitiveReverseDependencies else getReverseDependencies
    pkgs <- func (S.fromList args) idx utcTime
    let pkgs' = M.delete (mkPackageName "acme-everything") pkgs
        allCounters :: M.Map PackageName Int
        allCounters = M.unionsWith (+) $ fmap (fmap (const (1 :: Int))) pkgs'
        counters = map (\pkg -> M.findWithDefault 0 pkg allCounters) args
    putStrLn $ unwords $ show date : zipWith (curry showPair) args counters
    pure (date, counters)
  mTermWidth <- fmap (fmap TermSize.width) TermSize.size
  let plot = case mTermWidth of
        Nothing -> defPlot
        Just w -> defPlot { widthChars = max (widthChars defPlot) (w - 16) }
      yMax = fromIntegral $ maximum $ 0 : concatMap snd results
      plot' = plot { yBounds = (Just 0, Just yMax), xFormatter = const formatAsDate, yFormatter = const formatAsInt }
  let graphLines = zip (map (T.pack . unPackageName) args) (uncurry (\ds -> map (zip (map (fromIntegral . fromEnum) ds) . map fromIntegral)) $ second L.transpose $ unzip results)
      graph = lineGraph graphLines plot'
  putStrLn ""
  T.putStrLn graph

formatAsDate :: Int -> Double -> Text
formatAsDate n x = T.take (n - 1) $ T.pack $ show d
  where
    d :: Day
    d = toEnum (truncate x)

formatAsInt :: Int -> Double -> Text
formatAsInt _ = T.pack . show . truncate @Double @Int

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
