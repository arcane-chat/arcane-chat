{ runCommand, haskell }:

let
  paths = p: with p; [ shake shake-language-c ];
  ghc = haskell.packages.ghc7103.ghcWithPackages paths;
in runCommand "chat-shaker" { buildInputs = [ ghc ]; } ''
  mkdir -pv $out/bin
  ghc ${./chat-shaker.hs} -o $out/bin/chat-shaker
''