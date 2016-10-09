module Main where

import           Control.Applicative
import           Control.Arrow

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Language.C
import           Development.Shake.Language.C.Host
import           Development.Shake.Language.C.PkgConfig
import           Development.Shake.Language.C.ToolChain
import           Development.Shake.Util

import           Data.Label                             (get, set)
import           Data.Maybe
import           Debug.Trace

import           System.Console.GetOpt

type BFMutator = BuildFlags -> BuildFlags

composeBFMutators :: [Action BFMutator] -> Action BFMutator
composeBFMutators = fmap (foldl (>>>) id) . sequence

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

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

cxx14 :: Action (BFMutator)
cxx14 = return $ append compilerFlags [(Nothing, [ "-std=c++14" ])]

customInclude :: String -> String -> Action (BFMutator)
customInclude envvar postfix = do
    storePath <- getEnv envvar
    let dir = fromMaybe "ERR" storePath
    return $ append systemIncludes [ dir </> postfix ]

main :: IO ()
main = shakeArgsWith soptions options $ \flags targets -> return $ Just $ do
  let inFlags x = x `elem` flags

  let (_, nativeToolchain) = Development.Shake.Language.C.Host.defaultToolChain

  let crossToolchain = do Just cc <- getEnv "CC"
                          Just cxx <- getEnv "CXX"
                          nt <- nativeToolchain
                          nt |> set linkerCommand cxx
                             |> set compilerCommand cc
                             |> return

  let toolchain = if inFlags Main.Windows
                  then crossToolchain
                  else nativeToolchain

  let exe = if inFlags Main.Windows then "exe" else ""

  let debugOption = if inFlags Debug
                    then []
                    else [ return $ append defines [("QT_NO_DEBUG", Nothing)] ]

  let loadPkgConfig = pkgConfig defaultOptions

  let extraIncludeDirs = do
        composeBFMutators $
          (uncurry customInclude <$>
          ([ ("libsigcxx", "include/sigc++-2.0")
          , ("libsigcxx", "lib/sigc++-2.0/include")
          , ("glibmmdev", "include/giomm-2.4")
          , ("glibmmdev", "include/glibmm-2.4")
          , ("glibmm", "lib/giomm-2.4/include")
          , ("glibmmdev", "lib/glibmm-2.4/include")
          , ("gstreamermmdev", "include/gstreamermm-1.0")
          , ("gstreamermm", "lib/gstreamermm-1.0/include")
          ])) ++ [ return $ append userIncludes ["_build"] ]++ debugOption

  let qObject name = [ "src" </> name <.> "cpp"
                     , "_build/moc_" ++ name <.> "cpp" ]

  let client_cs = do let qs = [ "audiocall", "core", "friend"
                              , "utils", "channel", "kisscache"
                              , "kiss", "toxsink", "channelmodel"
                              , "mainwindow", "chatwidget"
                              , "infowidget", "callcontrol" ]

                     let cs = [ "src/client.cpp"
                              , "src/db.cpp"
                              , "src/stats.cpp"
                              , "src/core_db.cpp"
                              , "src/options.cpp"
                              , "_build/network.pb.cc"
                              ] ++ concatMap qObject qs

                     need $ [ "_build/network.pb.h"
                            , "_build/ui_mainwindow.h"
                            , "_build/ui_infowidget.h"
                            , "_build/ui_chatwidget.h"
                            ] ++ cs

                     return $ cs

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
    let dest = fromMaybe "/ERR" maybeDest
    copyFile' ("_build/arcane-chat" <.> exe) (dest </> "bin/arcane-chat" <.> exe)
    copyFile' "shakeReport" $ dest </> "shake/report.html"

  ["_build/network.pb.h", "_build/network.pb.cc"] |%> \out -> do
    need ["network.proto"]
    cmd "protoc --cpp_out=_build network.proto"

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

  let pkgConfigSet = loadPkgConfig <$> [ "Qt5GStreamer-1.0"
                    , "Qt5Core", "Qt5Widgets", "Qt5Network", "Qt5Script"
                    , "glib-2.0", "gstreamer-1.0", "gstreamermm-1.0"
                    , "gstreamer-audio-1.0", "sqlite3", "protobuf"
                    , "Qt5GLib-2.0", "Qt5GStreamerUtils-1.0"
                    , "libtoxcore", "libsodium" ]

  arcaneChat <- (executable toolchain ("_build/arcane-chat" <.> exe)
                 (composeBFMutators $
                  [ fmap (>>> traceShowId) $ loadPkgConfig "glibmm-2.4"
                  , cxx14, extraIncludeDirs
                  , return $ append defines [("ARCANE_CHAT_VERSION", Just "0")]
                  ] ++ pkgConfigSet)
                 client_cs)

  want $ if null targets then [ arcaneChat ] else targets
