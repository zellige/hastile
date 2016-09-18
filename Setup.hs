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
  --error (show [ mapnikInclude, mapnikLibDirs])
  dir <- getCurrentDirectory
  let updBinfo bi = bi { extraLibDirs = extraLibDirs bi ++ mapnikLibDirs ++ [dir ++ "mapnik-vector-tile-c"]
                       , extraLibs    = extraLibs    bi ++ mapnikLibs
                       , includeDirs  = includeDirs  bi ++ mapnikInclude
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
