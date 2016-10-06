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
-- import 

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    let
        (_, toolchain) = Development.Shake.Language.C.Host.defaultToolChain
        qtCore = pkgConfig defaultOptions "Qt5Core"
        qtWidgets = pkgConfig defaultOptions "Qt5Widgets"
        cs = getDirectoryFiles "" ["src//*.cpp"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    protostuff <- do
        -- fuck off want ["network.proto"]
        cmd "protoc"

    arcaneChat <- do
        want [ protostuff ]
        executable toolchain "_build/arcane-chat"
            (qtCore >> qtWidgets)
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
