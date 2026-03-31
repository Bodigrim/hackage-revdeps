-- | Functions to list Hackage reverse dependencies.
module Hackage.RevDeps (
  lastVersionsOfPackagesWithNeedles,
  lastVersionsOfPackages,
  extractDependencies,
  extractAllDependencies,
  getReverseDependencies,
  getTransitiveReverseDependencies,
  ExtractDependenciesMode (..),
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as Gzip
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Char (isPunctuation, isSpace, ord)
import Data.Foldable (fold)
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.AhoCorasick.Automaton qualified as Aho
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Unsafe qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Distribution.Compat.Lens (toListOf)
import Distribution.Compat.NonEmptySet qualified as NES (singleton)
import Distribution.PackageDescription (libBuildInfo, library)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Simple.BuildToolDepends (getAllToolDependencies)
import Distribution.Types.BuildInfo (BuildInfo, buildable, targetBuildDepends)
import Distribution.Types.BuildInfo.Lens qualified as Lens
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Distribution.Types.VersionRange (VersionRange)
import Distribution.Version (Version, intersectVersionRanges, mkVersion, simplifyVersionRange)
import System.FilePath (isPathSeparator)

-- | Scan Cabal index and return Cabal files
-- of last versions (not necessarily latest releases or revisions), which
-- contain one of the needles as an entire word (separated by spaces
-- or punctuation).
--
-- To avoid ambiguity: we first select the last versions,
-- then filter them by needles.
--
-- @since 0.3
lastVersionsOfPackagesWithNeedles
  :: (PackageName -> Bool)
  -- ^ Which packages are we looking for? Could be 'const' 'True'.
  -> Set ByteString
  -- ^ Needles to search in Cabal files.
  -> FilePath
  -- ^ Path to @01-index.tar@ or @01-index.tar.gz@.
  -> Maybe UTCTime
  -- ^ Timestamp of index state at which to stop scanning.
  -> IO (Map PackageName ByteString)
  -- ^ Map from packages with largest versions to their Cabal files.
lastVersionsOfPackagesWithNeedles pkgPred needles idx indexState =
  -- There is a memory vs. speed trade off: instead of collecting all last
  -- versions into a massive map (~300 M) and then filtering them,
  -- we could have filtered them as we go. The downside would be
  -- filtering each and every Cabal file, even for old versions
  -- which are soon to be rewritten by newer releases.
  filterByNeedles needles <$> lastVersionsOfPackages pkgPred idx indexState

filterByNeedles
  :: Set ByteString
  -> Map k ByteString
  -> Map k ByteString
filterByNeedles needles = M.filter (containsAnyAsWholeWord machine . decodeUtf8Lenient)
  where
    machine = Aho.build (map ((\x -> (x, x)) . decodeUtf8Lenient) (S.toList needles))

-- | Scan Cabal index @01-index.tar@ (or @01-index.tar.gz@) and return Cabal files
-- of last versions (not necessarily latest releases or revisions).
--
-- @since 0.3
lastVersionsOfPackages
  :: (PackageName -> Bool)
  -- ^ Which packages are we looking for? Could be 'const' 'True'.
  -> FilePath
  -- ^ Path to @01-index.tar@ or @01-index.tar.gz@.
  -> Maybe UTCTime
  -- ^ Timestamp of index state at which to stop scanning.
  -> IO (Map PackageName ByteString)
  -- ^ Map from packages with largest versions to their Cabal files.
lastVersionsOfPackages pkgPred idx indexState =
  fmap (fmap snd) $
    foldCabalFilesInIndex idx indexState mempty go
  where
    go pkg ver cnt = if pkgPred pkg then M.alter (Just . f) pkg else id
      where
        new = (ver, cnt)
        f = maybe new (\old@(ver', _) -> if ver' <= ver then new else old)

containsAnyAsWholeWord :: Aho.AcMachine Text -> Text -> Bool
containsAnyAsWholeWord machine hay = Aho.runText False go machine hay
  where
    isWordBoundary c = (isSpace c || isPunctuation c || c `elem` "^>=<") && c /= '-'

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
  -> Maybe UTCTime
  -> a
  -> (PackageName -> Version -> ByteString -> a -> a)
  -> IO a
foldCabalFilesInIndex fp indexState ini action = do
  contents' <- BL.readFile fp
  -- 1F 8B is GZip magic header, see https://en.wikipedia.org/wiki/Gzip#File_structure
  let isGzip = BL.pack [0x1f, 0x8b] `BL.isPrefixOf` contents'
      contents = if isGzip then Gzip.decompress contents' else contents'
  let entries' = Tar.read contents
      entries = case indexState of
        Nothing -> entries'
        Just t ->
          tarTakeWhile
            (\e -> fromIntegral (Tar.entryTime e) <= utcTimeToPOSIXSeconds t)
            entries'
  case Tar.foldlEntries go ini entries of
    Left (err, _) -> throwIO err
    Right res -> pure res
  where
    go acc entry =
      case Tar.entryContent entry of
        Tar.NormalFile contents _ ->
          if isCabalFile then action pkgName version bs acc else acc
          where
            -- If we do not force 'bs' at this point, we'll end up
            -- retaining entire 'contents' in memory, which is ~1G.
            !bs = BL.toStrict contents
            fpath = Tar.entryPath entry
            isCabalFile = ".cabal" `isSuffixOf` fpath
            (rawPkgName, fpath') = break isPathSeparator fpath
            pkgName = mkPackageName rawPkgName
            rawVersion = takeWhile (not . isPathSeparator) $ dropWhile isPathSeparator fpath'
            version = mkVersion $ readVersion rawVersion
        _ -> acc

readVersion :: String -> [Int]
readVersion = (\(acc, _mult, rest) -> acc : rest) . foldr go (0, 1, [])
  where
    go c (acc, mult, rest)
      | fromIntegral d < (10 :: Word) = (acc + d * mult, mult * 10, rest)
      | otherwise = (0, 1, acc : rest)
      where
        d = ord c - ord '0'

tarTakeWhile
  :: (Tar.Entry -> Bool)
  -> Tar.Entries a
  -> Tar.Entries a
tarTakeWhile p =
  Tar.foldEntries
    (\x xs -> if p x then Tar.Next x xs else Tar.Done)
    Tar.Done
    Tar.Fail

-- | Scan Cabal file looking for package names matching needles,
-- coalescing version bounds from all components and under all conditions.
--
-- @since 0.1
extractDependencies
  :: ExtractDependenciesMode
  -- ^ Which package components to scan?
  -> Set PackageName
  -- ^ Needles to search.
  -> ByteString
  -- ^ Content of a Cabal file.
  -> Map PackageName VersionRange
  -- ^ Needles found in the Cabal file and their version bounds.
extractDependencies mode needles = relevantDeps (`S.member` needles) . extractDeps mode

-- | Scan Cabal file looking for all dependencies,
-- coalescing version bounds from all components and under all conditions.
--
-- @since 0.3
extractAllDependencies
  :: ExtractDependenciesMode
  -- ^ Which package components to scan?
  -> ByteString
  -- ^ Content of a Cabal file.
  -> Map PackageName VersionRange
  -- ^ All dependencies found in the Cabal file and their version bounds.
extractAllDependencies mode = relevantDeps (const True) . extractDeps mode

-- | Which package components to look inside for dependencies?
--
-- @since 0.4
data ExtractDependenciesMode
  = -- | All buildable components, including tests, benchmarks, sublibraries.
    AllComponents
  | -- | Only the main library if any.
    OnlyMainLib

extractDeps :: ExtractDependenciesMode -> ByteString -> [Dependency]
extractDeps mode cnt = case parseGenericPackageDescriptionMaybe cnt of
  Nothing -> mempty
  Just descr -> foldMap (extractDepsFromBuildInfo getToolDeps) buildInfos
    where
      flatDescr = flattenPackageDescription descr
      getToolDeps = getAllToolDependencies flatDescr
      buildInfos = case mode of
        AllComponents -> toListOf Lens.traverseBuildInfos flatDescr
        OnlyMainLib -> maybeToList (fmap libBuildInfo (library flatDescr))

extractDepsFromBuildInfo :: (BuildInfo -> [ExeDependency]) -> BuildInfo -> [Dependency]
extractDepsFromBuildInfo getToolDeps bi
  | buildable bi =
      targetBuildDepends bi
        <> map
          (\(ExeDependency pkg _ ver) -> Dependency pkg ver (NES.singleton LMainLibName))
          (getToolDeps bi)
  | otherwise = []

relevantDeps :: (PackageName -> Bool) -> [Dependency] -> Map PackageName VersionRange
relevantDeps predicate =
  fmap simplifyVersionRange . M.fromListWith intersectVersionRanges . mapMaybe go
  where
    go (Dependency pkg ver _)
      | predicate pkg = Just (pkg, ver)
      | otherwise = Nothing

-- | Combination of 'lastVersionsOfPackagesWithNeedles' and 'extractDependencies'.
--
-- @since 0.2
getReverseDependencies
  :: ExtractDependenciesMode
  -- ^ Which package components to scan?
  -> Set PackageName
  -- ^ Needles to search in Cabal files.
  -> FilePath
  -- ^ Path to @01-index.tar@ or @01-index.tar.gz@.
  -> Maybe UTCTime
  -- ^ Timestamp of index state at which to stop scanning.
  -> IO (Map PackageName (Map PackageName VersionRange))
  -- ^ Outer keys are reverse dependencies,
  -- inner keys are needles,
  -- inner values are accepted version ranges.
getReverseDependencies mode args fp indexState = do
  releases <-
    lastVersionsOfPackagesWithNeedles
      (const True)
      (S.map (B.pack . unPackageName) args)
      fp
      indexState
  pure $ extractDependencies' args $ fmap (extractDeps mode) releases

extractDependencies'
  :: Set PackageName
  -> Map PackageName [Dependency]
  -> Map PackageName (Map PackageName VersionRange)
extractDependencies' args releases =
  M.filter (not . null) $
    -- Most of packages trivially depend on themselves
    -- (e. g., test suite depends on a library); skip it.
    M.mapWithKey M.delete $
      fmap (relevantDeps (`S.member` args)) releases

-- | Same as 'getReverseDependencies', but
-- returns not only direct, but also indirect reverse dependencies.
--
-- @since 0.2
getTransitiveReverseDependencies
  :: ExtractDependenciesMode
  -- ^ Which package components to scan?
  -> Set PackageName
  -- ^ Needles to search in Cabal files.
  -> FilePath
  -- ^ Path to @01-index.tar@ or @01-index.tar.gz@.
  -> Maybe UTCTime
  -- ^ Timestamp of index state at which to stop scanning.
  -> IO (Map PackageName (Map PackageName VersionRange))
  -- ^ Reverse dependencies, including transitive ones.
getTransitiveReverseDependencies mode args fp indexState = do
  releases <- lastVersionsOfPackages (const True) fp indexState
  go mempty (fmap (extractDeps mode) releases) args
  where
    go
      :: Map PackageName (Map PackageName VersionRange)
      -> Map PackageName [Dependency]
      -> Set PackageName
      -> IO (Map PackageName (Map PackageName VersionRange))
    go acc releases xs = do
      let rawRevDeps = extractDependencies' xs releases
          revDeps = M.map (\ys -> ys <> fold (M.restrictKeys acc (M.keysSet ys))) rawRevDeps
          revDepsKeys = M.keysSet revDeps
      if revDepsKeys `S.isSubsetOf` M.keysSet acc
        then pure acc
        else
          go
            (M.unionWith (M.unionWith intersectVersionRanges) acc revDeps)
            (M.withoutKeys releases revDepsKeys)
            revDepsKeys
