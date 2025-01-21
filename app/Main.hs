{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
  main,
) where

import Data.ByteString.Char8 qualified as B
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Time (Day, UTCTime (..))
import Distribution.Client.Config (loadConfig, savedGlobalFlags)
import Distribution.Client.GlobalFlags (globalCacheDir)
import Distribution.Compat.NonEmptySet (singleton)
import Distribution.Pretty (pretty)
import Distribution.Simple.Flag (fromFlag)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.VersionRange (VersionRange)
import Hackage.RevDeps (extractDependencies, latestReleases)
import Options.Applicative (Parser, ReadM, auto, execParser, fullDesc, help, helper, info, long, metavar, option, optional, progDesc, strArgument)
import Options.Applicative.NonEmpty (some1)
import System.Console.ANSI (hSupportsANSI, hyperlinkCode)
import System.FilePath ((</>))
import System.IO (stdout)

data Config = Config
  { cnfIndexState :: !(Maybe UTCTime)
  , cnfPackageNames :: !(NonEmpty PackageName)
  }

parseArgs :: Parser Config
parseArgs = do
  cnfIndexState <-
    optional $
      option (fmap (`UTCTime` 0) (auto :: ReadM Day)) $
        long "index-state"
          <> help "Timestamp of index state at which to stop scanning, YYYY-MM-DD"
  cnfPackageNames <-
    some1 $
      strArgument $
        metavar "PKGS"
          <> help "Package names to scan Hackage for their reverse dependencies"
  pure Config {..}

main :: IO ()
main = do
  let desc = "List Hackage reverse dependencies, using local package index. Consider running 'cabal update' beforehand."
  Config {..} <-
    execParser $
      info (helper <*> parseArgs) (fullDesc <> progDesc desc)
  let args = NE.toList cnfPackageNames

  cnf <- loadConfig minBound mempty
  let cacheDir = fromFlag $ globalCacheDir $ savedGlobalFlags cnf
      idx = cacheDir </> hackageHaskellOrg </> "01-index.tar"
      needles = map (B.pack . unPackageName) args
  releases <- latestReleases needles idx cnfIndexState
  let pkgs = fmap (extractDependencies args) releases
      pkgs' = M.mapWithKey M.delete pkgs
  report $ M.filter (not . null) pkgs'

report :: Map PackageName (Map PackageName VersionRange) -> IO ()
report pkgs
  | M.null pkgs = putStrLn "No reverse dependencies found"
  | otherwise = do
      supportsAnsi <- hSupportsANSI stdout
      putStrLn "Reverse dependencies:"
      let prettify (k, v) = pretty $ Dependency k v (singleton LMainLibName)
          pkgs' = fmap (map prettify . M.toAscList) pkgs
      reportTable supportsAnsi pkgs'
      putStrLn "Total count:"
      let counters = M.unionsWith (+) $ fmap (fmap (const (1 :: Int))) pkgs
      reportTable supportsAnsi counters

reportTable :: Show v => Bool -> Map PackageName v -> IO ()
reportTable supportsAnsi kvs = putStrLn $ unlines $ map showPair $ M.toAscList kvs
  where
    longestKey = maximum $ 0 : map (length . unPackageName) (M.keys kvs)
    showPair (k, v) =
      showPackage supportsAnsi k
        ++ replicate (longestKey + 1 - length (unPackageName k)) ' '
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
