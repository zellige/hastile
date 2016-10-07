-- Lovingly stolen from hs-mapnik
import Control.Monad
import Data.Maybe
import System.Process
import System.IO
import System.Environment (lookupEnv)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Exit
import Data.List
import Distribution.Simple
import Distribution.Simple.Setup (configConfigurationsFlags)
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

main = defaultMainWithHooks simpleUserHooks {confHook = mapnikConf}

mapnikConf (pkg0, pbi) flags = do
 lbi <- confHook simpleUserHooks (pkg0, pbi) flags
 configureWithMapnikConfig lbi

configureWithMapnikConfig lbi = do
  -- else we get link errors in client in client libs/apps
  let noStatic = filter (/="-static")
  mapnikInclude <- mapM makeAbsolute =<< liftM (getFlagValues 'I')
    (mapnikConfig ["--includes", "--dep-includes"])
  mapnikLibDirs <- mapM makeAbsolute =<< liftM (getFlagValues 'L')
    (mapnikConfig ["--libs", "--dep-libs", "--ldflags"])
  mapnikLibs    <- liftM (getFlagValues 'l')
    (mapnikConfig ["--libs", "--dep-libs", "--ldflags"])
  mapnikCcOptions <- liftM (noStatic . words) $
    (mapnikConfig ["--defines", "--cxxflags"])
  mapnikLdOptions <- liftM (noStatic . words)
    (mapnikConfig ["--ldflags"])
  mapnikInputPluginDir <- liftM (escapeWinPathSep . head . words)
    (mapnikConfig ["--input-plugins"])
  mapnikFontDir <- liftM (escapeWinPathSep . head . words) $
    (mapnikConfig ["--fonts"])
  -- We require the user to specify where mapnik-vector-tile source is. Lib should
  -- be built in the standard location.
  mvtSrc <- getMvtSource
  mvtIncludes <- sequence . fmap makeAbsolute $ getMvtIncludes mvtSrc
  mvtLibDirs <- makeAbsolute $ mvtSrc ++ "/build/Release"
  --error (show [ mapnikInclude, mapnikLibDirs])
  dir <- getCurrentDirectory
  let updBinfo bi = bi { extraLibDirs = extraLibDirs bi ++ mapnikLibDirs ++ [mvtLibDirs]
                       , extraLibs    = extraLibs    bi ++ mapnikLibs ++ ["mapnik_vector_tile_impl"]
                       , includeDirs  = includeDirs  bi ++ mapnikInclude ++ mvtIncludes
                       , ccOptions    = ccOptions    bi ++ mapnikCcOptions
                       , ldOptions    = ldOptions    bi ++ mapnikLdOptions
                       , cppOptions   = cppOptions   bi ++ mapnikCppOptions
                       }
      mapnikCppOptions =
        [ "-DDEFAULT_FONT_DIR=\""         ++ mapnikFontDir ++ "\""
        , "-DDEFAULT_INPUT_PLUGIN_DIR=\"" ++ mapnikInputPluginDir ++ "\""
        ]
      updLib lib = lib { libBuildInfo  = updBinfo (libBuildInfo lib)}
      updTs  ts  = ts  { testBuildInfo = updBinfo (testBuildInfo ts)}
      updBm  bm  = bm  { benchmarkBuildInfo = updBinfo (benchmarkBuildInfo bm)}
      updExe ex  = ex  { buildInfo     = updBinfo (buildInfo ex)}
      updLpd lpd = lpd { library       = fmap updLib (library lpd)
                       , testSuites    = map updTs (testSuites lpd)
                       , benchmarks    = map updBm (benchmarks lpd)
                       , executables   = map updExe (executables lpd)
                       }
  let lbi' = lbi { localPkgDescr = updLpd (localPkgDescr lbi) }
  return lbi'

getMvtIncludes :: String -> [String]
getMvtIncludes mvtSrc =  fmap (mvtSrc ++) ["/src", "/deps/protozero/include", "/deps/clipper/cpp"]

getMvtSource :: IO String
getMvtSource = lookupEnv "MAPNIK_VECTOR_TILE_SRC" >>= maybe bad pure
  where bad = error "Must set MAPNIK_VECTOR_TILE_SRC to source checkout with static library already built"

getOutput s a = readProcess s a ""

getFlagValues f = map (\(_:_:v) -> v)
                . filter (\(_:f':_) -> f==f')
                . words

escapeWinPathSep = concatMap go
  where go '\\' = "\\\\"
        go x   = [x]

mapnikConfig args = do
  mCmd <- lookupEnv "MAPNIK_CONFIG"
  cmd <- maybe (liftM init (getOutput "bash" ["-c", "which mapnik-config"])) return mCmd
  getOutput "bash" (cmd:args)
