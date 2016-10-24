{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Aeson                             (FromJSON, Object,
                                                         Value (..), decode,
                                                         parseJSON, (.:))
import qualified Data.Yaml                              as Yaml

import           Control.Applicative
import           Control.Arrow
import           Control.Monad                          (when)

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Language.C
import           Development.Shake.Language.C.Host
import           Development.Shake.Language.C.PkgConfig
import           Development.Shake.Language.C.ToolChain
import           Development.Shake.Util

import           Data.ByteString                        (ByteString, readFile)
import           Data.Label                             (get, set)
import           Data.Map.Strict                        (Map)
import qualified Data.Map.Strict                        as Map
import           Data.Maybe
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import           Debug.Trace

import           GHC.Generics

import           System.Console.GetOpt

data Project =
  MkProject
  { executables :: Map String Executable
  } deriving (Show)

instance FromJSON Project where
  parseJSON (Object o) = do execs <- (o .: Text.pack "executables")
                            MkProject <$> parseJSON execs
  parseJSON _          = empty

data Executable =
  MkExecutable
  { sources  :: [String]
  , required_headers :: [String]
  , pkg_config :: [String]
  , libs :: [String]
  } deriving (Generic, Show)

instance FromJSON Executable

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

type BFMutator = BuildFlags -> BuildFlags

composeBFMutators :: [Action BFMutator] -> Action BFMutator
composeBFMutators = fmap (foldl (>>>) id) . sequence

whenBFM :: Bool -> Action BFMutator -> Action BFMutator
whenBFM True  m = m
whenBFM False _ = pure id

data Flag = Debug | Windows deriving (Show, Eq)

options :: [ OptDescr (Either String Flag) ]
options = [ Option [] ["debug-build"] (NoArg $ Right Debug) "do a debug build"
          , Option [] ["windows"] (NoArg $ Right Main.Windows) "do a windows build"
          ]

soptions :: ShakeOptions
soptions = shakeOptions { shakeFiles = "_build"
                        , shakeReport = [ "shakeReport" ]
                        , shakeProgress = progressSimple
                        }

cxx14 :: Action BFMutator
cxx14 = pure $ append compilerFlags [(Nothing, [ "-std=c++14" ])]

customInclude :: String -> String -> Action BFMutator
customInclude envvar postfix = do
  storePath <- getEnv envvar
  let dir = fromMaybe "ERR" storePath
  pure $ append systemIncludes [ dir </> postfix ]

parseData :: ByteString -> Maybe Project
parseData = Yaml.decode

get_cs :: Executable -> Action [ FilePath ]
get_cs entry = do
    need $ required_headers entry
    pure $ sources entry

main :: IO ()
main = shakeArgsWith soptions options $ \flags targets -> pure $ Just $ do
  sampleData <- liftIO $ Data.ByteString.readFile "input.yaml"
  let Just parsed2 = parseData sampleData
  let inFlags x = x `elem` flags

  phony "test-decode" $ do
    liftIO $ putStrLn $ show parsed2

  let (_, nativeToolchain) = Development.Shake.Language.C.Host.defaultToolChain

  let crossToolchain = do Just cc <- getEnv "CC"
                          Just cxx <- getEnv "CXX"
                          nt <- nativeToolchain
                          nt |> set linkerCommand cxx
                             |> set compilerCommand cc
                             |> pure

  let toolchain = if inFlags Main.Windows
                  then crossToolchain
                  else nativeToolchain

  let exe = if inFlags Main.Windows then "exe" else ""

  let debugOption = if inFlags Debug
                    then []
                    else [ pure $ append defines [("QT_NO_DEBUG", Nothing)] ]

  let loadPkgConfig = pkgConfig defaultOptions

  let extraIncludeDirs = composeBFMutators $
                         (uncurry customInclude <$>
                          ([ ("libsigcxx", "include/sigc++-2.0")
                           , ("libsigcxx", "lib/sigc++-2.0/include")
                           , ("glibmmdev", "include/giomm-2.4")
                           , ("glibmmdev", "include/glibmm-2.4")
                           , ("glibmm", "lib/giomm-2.4/include")
                           , ("glibmmdev", "lib/glibmm-2.4/include")
                           , ("gstreamermmdev", "include/gstreamermm-1.0")
                           , ("gstreamermm", "lib/gstreamermm-1.0/include")
                           ]))
                         ++ [ pure $ append userIncludes ["_build"] ]
                         ++ debugOption

  let qObject name = [ "src" </> name <.> "cpp"
                     , "_build/moc_" ++ name <.> "cpp" ]

  "_build//ui_*.h" %> \out -> do
    let name = drop 3 (dropDirectory1 out) -<.> "ui"
    let src = "src" </> name
    need [ src ]
    cmd "uic -o" out src

  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  phony "install" $ do
    maybeDest <- getEnv "out"
    maybeReport <- getEnv "report";
    let dest = fromMaybe "/ERR" maybeDest
    let dest2 = fromMaybe "/ERR" maybeReport
    let installExec name = copyFile'
                           ("_build" </> name <.> exe)
                           (dest </> "bin" </> name <.> exe)
    executables parsed2
      |> Map.keys
      |> mapM_ installExec
    copyFile' "shakeReport" $ dest2 </> "shake/report.html"

  "_build//*.pb.h" %> \out -> do
    let proto = dropExtension (dropDirectory1 out) -<.> "proto"
    need ["proto/" ++ proto]
    command_ [Cwd "proto"] "protoc" ["--cpp_out=../_build", proto]

  "_build//*.pb.cc" %> \out -> need [out -<.> "h"]

  "_build//*.moc" %> \out -> do
    let name = dropDirectory1 $ out -<.> "cpp"
    let src = "src" </> name
    need [ src ]
    cmd "moc " src "-o" out

  "_build//moc_*.cpp" %> \out -> do
    let name = drop 4 $ dropDirectory1 $ out -<.> "hpp"
    let src = ("src" </> name)
    need [ src ]
    cmd "moc " src "-o" out

  let pkgConfigSet entry = loadPkgConfig <$> pkg_config entry

  let simple_prog name entry =
        executable toolchain ("_build" </> name <.> exe)
          (composeBFMutators $ concat
           [ [ (>>> traceShowId) <$> loadPkgConfig "glibmm-2.4"
             , cxx14, extraIncludeDirs
             , pure $ append defines [("ARCANE_CHAT_VERSION", Just "0")]
             ]
           , pkgConfigSet entry
           -- , [ pure $ append compilerFlags [(Nothing, ["-gdwarf-2", "-gstrict-dwarf"])] ]
           , [ pure $ append libraries $ libs entry
             , whenBFM (inFlags Main.Windows) $ pure $ append libraries
               [ "orc-0.4", "ws2_32", "ole32", "iphlpapi", "dnsapi", "winmm" ]
             , pure $ append libraries [ "z", "pcre", "ffi", "pthread" ]
             ]
           ])
          (get_cs entry)

  allTargets <- executables parsed2
                |> Map.mapWithKey simple_prog
                |> Map.toList
                |> mapM snd

  want $ if null targets then allTargets else targets
