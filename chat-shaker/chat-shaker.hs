{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Aeson (decode, FromJSON, Object, parseJSON)

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

import           Data.ByteString.Lazy (ByteString, readFile)
import           Data.Label                             (get, set)
import           Data.Maybe
import           Debug.Trace

import           GHC.Generics

import           System.Console.GetOpt

data Project = Project {
      executables :: [ Executable ] -- todo: change to a name->Executable map
    } deriving (Generic, Show)

instance FromJSON Project

data Executable = Executable {
      name :: String
    , sources  :: [ String ]
    , required_headers :: [ String ]
    , pkg_config :: [ String ]
    , libs :: [ String ]
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
parseData = decode

hack :: Executable -> (String, Executable)
hack e = (name e, e)

get_cs :: Executable -> Action [ FilePath ]
get_cs entry = do
    need $ required_headers entry
    pure $ sources entry

main :: IO ()
main = shakeArgsWith soptions options $ \flags targets -> pure $ Just $ do
  sampleData <- liftIO $ Data.ByteString.Lazy.readFile "input.json"
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
    mapM_ (\e -> copyFile' ("_build" </> (name e) <.> exe) (dest </> "bin" </> (name e) <.> exe)) $ executables parsed2
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
    let src = "src" </> name
    need [ src ]
    cmd "moc " src "-o" out

  let pkgConfigSet entry = loadPkgConfig <$> pkg_config entry

  let simple_prog name = do
              let
                Just entry = lookup name $ map hack $ executables parsed2
              executable toolchain ("_build" </> name <.> exe)
                 (composeBFMutators $ concat
                  [ [ fmap (>>> traceShowId) $ loadPkgConfig "glibmm-2.4"
                    , cxx14, extraIncludeDirs
                    , pure $ append defines [("ARCANE_CHAT_VERSION", Just "0")]
                    ]
                  , pkgConfigSet entry
                  -- , [ pure $ append compilerFlags [(Nothing, [  "-gdwarf-2", "-gstrict-dwarf" ])] ]
                  , [ pure $ append libraries $ libs entry
                    , whenBFM (inFlags Main.Windows) $ pure $ append libraries
                      [ "orc-0.4", "ws2_32", "ole32", "iphlpapi", "dnsapi", "winmm" ]
                    , pure $ append libraries [ "z", "pcre", "ffi", "pthread" ]
                    ]
                  ])
                 (get_cs entry)

  arcaneChat <- simple_prog "arcane-chat"

  want $ if null targets then [ arcaneChat ] else targets
