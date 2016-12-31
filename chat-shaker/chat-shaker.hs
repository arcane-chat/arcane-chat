#!/usr/bin/env runhaskell
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Aeson
                 (FromJSON, Object, Value (..), decode, parseJSON)
import qualified Data.Aeson                             as Aeson
import qualified Data.Aeson.Types                       as Aeson

import qualified Data.Yaml                              as Yaml

import           Control.Applicative
import           Control.Arrow
import           Control.Monad                          (when)
import           Data.Monoid

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Language.C
import           Development.Shake.Language.C.Host
import           Development.Shake.Language.C.PkgConfig
import           Development.Shake.Language.C.ToolChain
import           Development.Shake.Util

import           Data.ByteString                        (ByteString)
import qualified Data.ByteString                        as BS
import           Data.Map.Strict                        (Map)
import qualified Data.Map.Strict                        as Map

import qualified Data.Text                              as Text

import           Data.Label                             (get, set)
import           Data.List.Split                        (splitOn)
import           Data.Maybe
import           Debug.Trace
import           GHC.Generics
import           System.Console.GetOpt

(.:) :: FromJSON a => Object -> String -> Aeson.Parser a
(.:) obj str = (Aeson..:) obj (Text.pack str)
infixl 9 .:

(.:?) :: FromJSON a => Object -> String -> Aeson.Parser (Maybe a)
(.:?) obj str = (Aeson..:?) obj (Text.pack str)
infixl 9 .:?

data Project =
  MkProject
  { projectLibraries   :: Map String CTarget
  , projectExecutables :: Map String CTarget
  } deriving (Eq, Show, Read)

instance FromJSON Project where
  parseJSON (Object o) = MkProject
                         <$> (fromMaybe mempty <$> o .:? "libraries")
                         <*> (fromMaybe mempty <$> o .:? "executables")
  parseJSON _          = empty

data CTarget =
  MkCTarget
  { ctSources         :: [String]
  , ctRequiredHeaders :: [String]
  , ctHeaders         :: [String]
  , ctPkgConfig       :: [String]
  , ctLibs            :: [String]
  } deriving (Eq, Show, Read, Generic)

instance FromJSON CTarget where
  parseJSON (Object o) = MkCTarget
                         <$> (o .: "sources")
                         <*> (o .: "required-headers")
                         <*> (o .: "headers")
                         <*> (o .: "pkg-config")
                         <*> (o .: "libraries")
  parseJSON _          = empty

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

type BFMutator = BuildFlags -> BuildFlags

composeBFMutators :: [Action BFMutator] -> Action BFMutator
composeBFMutators = fmap (foldl (>>>) id) . sequence

whenBFM :: Bool -> Action BFMutator -> Action BFMutator
whenBFM True  m = m
whenBFM False _ = pure id

data Flag = FlagDebugging | FlagWindows deriving (Show, Eq)

options :: [ OptDescr (Either String Flag) ]
options = [ Option [] ["debugging"] (NoArg $ Right FlagDebugging)
            "build with debugging enabled"
          , Option [] ["windows"] (NoArg $ Right FlagWindows)
            "do a windows build"
          ]

soptions :: ShakeOptions
soptions = shakeOptions { shakeFiles = "_build"
                        , shakeReport = ["shakeReport"]
                        , shakeProgress = progressSimple
                        }

cxx14 :: Action BFMutator
cxx14 = pure $ append compilerFlags [(Nothing, ["-std=c++14"])]

customInclude :: String -> String -> Action BFMutator
customInclude envvar postfix = do
  storePath <- getEnv envvar
  let dir = fromMaybe "ERR" storePath
  pure $ append systemIncludes [dir </> postfix]

parseData :: ByteString -> Maybe Project
parseData = Yaml.decode

get_cs :: CTarget -> Action [FilePath]
get_cs entry = do
  let moc_names = (\e -> ("_build/moc_" ++ last (splitOn "/" e)) -<.> "cpp")
                  <$> ctHeaders entry
  need $ ctRequiredHeaders entry
  pure $ ctSources entry ++ moc_names

main :: IO ()
main = shakeArgsWith soptions options $ \flags targets -> pure $ Just $ do
  sampleData <- liftIO $ BS.readFile "input.yaml"
  let Just parsed2 = parseData sampleData
  let inFlags x = x `elem` flags

  phony "test-decode" $ do
    liftIO $ print parsed2
    pure ()

  let (_, nativeToolchain) = Development.Shake.Language.C.Host.defaultToolChain

  let crossToolchain = do Just cc <- getEnv "CC"
                          Just cxx <- getEnv "CXX"
                          nt <- nativeToolchain
                          nt |> set linkerCommand cxx
                             |> set compilerCommand cc
                             |> pure

  let toolchain = if inFlags FlagWindows
                  then crossToolchain
                  else nativeToolchain

  let exe = if inFlags FlagWindows then "exe" else ""

  let debugOption = if inFlags FlagDebugging
                    then [ pure $ append compilerFlags
                           [ (Nothing, ["-g", "-Og"]) ] ]
                    else [ pure $ append defines
                           [ ("QT_NO_DEBUG", Nothing)
                           , ("NDEBUG",      Nothing) ] ]

  let loadPkgConfig = pkgConfig defaultOptions

  let extraIncludeDirs = composeBFMutators $
                         (uncurry customInclude <$>
                          [ ("LIBSIGCXX_OUT",   "include/sigc++-2.0")
                          , ("LIBSIGCXX_OUT",   "lib/sigc++-2.0/include")
                          , ("GLIBMM_DEV",      "include/giomm-2.4")
                          , ("GLIBMM_DEV",      "include/glibmm-2.4")
                          , ("GLIBMM_OUT",      "lib/giomm-2.4/include")
                          , ("GLIBMM_DEV",      "lib/glibmm-2.4/include")
                          , ("GSTREAMERMM_DEV", "include/gstreamermm-1.0")
                          , ("GSTREAMERMM_OUT", "lib/gstreamermm-1.0/include")
                          ])
                         ++ [ pure $ append userIncludes ["_build", "_build/psc"] ]
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
    Just dest <- getEnv "out"
    Just dest2 <- getEnv "report"
    let installExec name = copyFile'
                           ("_build" </> name <.> exe)
                           (dest </> "bin" </> name <.> exe)
    -- FIXME: add library install support here
    projectExecutables parsed2
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

  let pkgConfigSet entry = loadPkgConfig <$> ctPkgConfig entry

  let simple_prog name entry =
        executable toolchain ("_build" </> name <.> exe)
          (composeBFMutators $ concat
           [ [ (>>> traceShowId) <$> loadPkgConfig "glibmm-2.4"
             , cxx14, extraIncludeDirs
             , pure $ append defines [("ARCANE_CHAT_VERSION", Just "0")]
             ]
           , pkgConfigSet entry
           -- , [ pure $ append compilerFlags [(Nothing, ["-gdwarf-2", "-gstrict-dwarf"])] ]
           , [ pure $ append libraries $ ctLibs entry
             , whenBFM (inFlags FlagWindows) $ pure $ append libraries
               [ "orc-0.4", "ws2_32", "ole32", "iphlpapi", "dnsapi", "winmm" ]
             , pure $ append libraries [ "z", "pcre", "ffi", "pthread" ]
             ]
           ])
          (get_cs entry)

  -- FIXME: add library build support here
  libTargets <- pure []

  exeTargets <- projectExecutables parsed2
                |> Map.mapWithKey simple_prog
                |> Map.toList
                |> mapM snd

  let allTargets = libTargets <> exeTargets

  want $ if null targets then allTargets else targets


--  "_build/psc-package.json"

  "_build/psc-package.json" %> \_ -> do
    command_ [] "cp" ["-v", "psc-package.json", "_build"]
    command_ [] "mkdir" [ "-pv", "_build/.psc-package" ]
    Just foo <- getEnv "FOO"
    command_ [] "cp" ["-r", foo </> "psc-0.10.2", "_build/.psc-package/psc-0.10.2" ]
    depPS <- map ("_build/.psc-package/" <>)
             <$> getDirectoryFiles "_build/.psc-package" ["//*.purs"]
    srcPS <- map ("src/" <>)
             <$> getDirectoryFiles "src" ["//*.purs"]
    need (depPS <> srcPS)
    command_ [] "pcc" $ mconcat
      [["--comments", "--output", "_build/psc"], depPS, srcPS]

  "_build/psc//*.cc" %> \_ -> do
    -- command_ [Cwd "_build"] "psc-package" ["update"]
    need [ "_build/psc-package.json" ]
