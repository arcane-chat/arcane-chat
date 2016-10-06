module Main where

import Control.Applicative
import Control.Arrow

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Development.Shake.Language.C.PkgConfig
import Development.Shake.Language.C.ToolChain
import Development.Shake.Language.C.Host
import Development.Shake.Language.C

import Debug.Trace

composeBAMutators :: [Action (BuildFlags -> BuildFlags)] -> Action (BuildFlags -> BuildFlags)
composeBAMutators = fmap (foldl (>>>) id) . sequence

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    let
        (_, toolchain) = Development.Shake.Language.C.Host.defaultToolChain
        qtCore = pkgConfig defaultOptions "Qt5Core"
        qtWidgets = pkgConfig defaultOptions "Qt5Widgets"
        gst = pkgConfig defaultOptions "gstreamer-1.0"
        toxcore = pkgConfig defaultOptions "libtoxcore"
        cxx14 = return $ append compilerFlags [(Nothing, [ "-std=c++14" ])]
        includeDirs = return $ append userIncludes ["_build"]
        glib = pkgConfig defaultOptions "glib-2.0"
        glibmm = pkgConfig defaultOptions "glibmm-2.4"
        cs = do dirFiles <- getDirectoryFiles "" ["src//*.cpp"]
                need ["_build/network.pb.h"]
                return dirFiles

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    "_build/network.pb.h" %> \out -> do
        need ["network.proto"]
        cmd "protoc --cpp_out=_build network.proto"

    --"_build/arcane-chat" <.> exe %> \out -> do
    arcaneChat <- executable toolchain "_build/arcane-chat"
        (composeBAMutators [
            fmap (>>> traceShowId) glibmm, qtCore, qtWidgets,
            glib,
            gst,
            toxcore,
            includeDirs, cxx14
        ])
        cs
    want [ arcaneChat ]
    --let os = ["_build/src" </> c -<.> "o" | c <- cs]
    --need os
    --cmd "gcc -o" [out] os

    --"_build//*.o" %> \out -> do
        --let
            --c = dropDirectory1 $ out -<.> "cpp"
            --m = out -<.> "m"
        -- () <- cmd "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
        -- needMakefileDependencies m
