module Hackage.RevDeps (
  latestReleases,
  extractDependencies,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Index qualified as Tar
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isPunctuation, isSpace)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Strict.Tuple (Pair (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.AhoCorasick.Automaton qualified as Aho
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Unsafe qualified as T
import Distribution.Compat.Lens (toListOf)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Types.BuildInfo (targetBuildDepends)
import Distribution.Types.BuildInfo.Lens qualified as Lens
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.Types.VersionRange (VersionRange)
import Distribution.Version (intersectVersionRanges, simplifyVersionRange)
import System.FilePath (isPathSeparator)

-- | Scan Cabal index @01-index.tar@ and return Cabal files
-- of latest releases (not necessarily largest versions), which
-- contain one of the needles as an entire word (separated by spaces
-- or punctuation).
--
-- To avoid ambiguity: we first select the latest releases,
-- then filter them by needles.
latestReleases
  :: [ByteString]
  -- ^ Needles to search in Cabal files.
  -> FilePath
  -- ^ Path to @01-index.tar@.
  -- One can use @Cabal.Config.cfgRepoIndex@ from @cabal-install-parsers@
  -- to obtain it.
  -> IO (Map PackageName ByteString)
  -- ^ Map from latest releases to their Cabal files.
latestReleases needles idx =
  M.filter (containsAnyAsWholeWord machine . decodeUtf8Lenient)
    <$> getLatestReleases idx
  where
    machine = Aho.build (map ((\x -> (x, x)) . decodeUtf8Lenient) needles)

-- | Strip revisions and releases except the latest one.
getLatestReleases :: FilePath -> IO (Map PackageName ByteString)
getLatestReleases idx = foldCabalFilesInIndex idx mempty M.insert

containsAnyAsWholeWord :: Aho.AcMachine Text -> Text -> Bool
containsAnyAsWholeWord machine hay = Aho.runText False go machine hay
  where
    isWordBoundary c = isSpace c || isPunctuation c

    go :: Bool -> Aho.Match Text -> Aho.Next Bool
    go _ (Aho.Match pos val) =
      if startsWithBoundary && endsWithBoundary
        then Aho.Done True
        else Aho.Step False
      where
        pref =
          T.dropEnd (T.length val) $
            T.takeWord8 (fromIntegral (Aho.codeUnitIndex pos)) hay
        startsWithBoundary = maybe True (isWordBoundary . snd) (T.unsnoc pref)
        suff = T.dropWord8 (fromIntegral (Aho.codeUnitIndex pos)) hay
        endsWithBoundary = maybe True (isWordBoundary . fst) (T.uncons suff)

-- | Inspired by @Cabal.Index.foldIndex@ from @cabal-install-parsers@.
foldCabalFilesInIndex
  :: FilePath
  -> a
  -> (PackageName -> ByteString -> a -> a)
  -> IO a
foldCabalFilesInIndex fp ini action = do
  contents <- BL.readFile fp
  Right (_ :!: result) <- pure $ Tar.foldlEntries go (0 :!: ini) (Tar.read contents)
  pure result
  where
    go (offset :!: acc) entry =
      Tar.nextEntryOffset entry offset
        :!: case Tar.entryContent entry of
          Tar.NormalFile contents _ ->
            if isCabalFile then action pkgName bs acc else acc
            where
              bs = BL.toStrict contents
              fpath = Tar.entryPath entry
              isCabalFile = ".cabal" `isSuffixOf` fpath
              pkgName = mkPackageName $ takeWhile (not . isPathSeparator) fpath
          _ -> acc

-- | Scan Cabal file looking for package names,
-- coalescing version bounds from all components and under all conditions.
extractDependencies
  :: [PackageName]
  -- ^ Needles to search.
  -> ByteString
  -- ^ Content of a Cabal file.
  -> Map PackageName VersionRange
  -- ^ Needles found in the Cabal file and their version bounds.
extractDependencies needles = relevantDeps needles . extractDeps

extractDeps :: ByteString -> [Dependency]
extractDeps cnt = case parseGenericPackageDescriptionMaybe cnt of
  Nothing -> mempty
  Just descr -> foldMap targetBuildDepends $ toListOf Lens.traverseBuildInfos descr

relevantDeps :: [PackageName] -> [Dependency] -> Map PackageName VersionRange
relevantDeps needles =
  fmap simplifyVersionRange . M.fromListWith intersectVersionRanges . mapMaybe go
  where
    go (Dependency pkg ver _)
      | pkg `elem` needles = Just (pkg, ver)
      | otherwise = Nothing
