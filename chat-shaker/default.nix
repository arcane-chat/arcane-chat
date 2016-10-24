{ runCommand, haskellPackages }:

let paths = p: with p; [ shake shake-language-c aeson yaml ];
    ghc = haskellPackages.ghcWithPackages paths;
in runCommand "chat-shaker" { buildInputs = [ ghc ]; } ''
    mkdir -pv $out/bin
    cp -vi ${./chat-shaker.hs} chat-shaker.hs
    ghc -O2 chat-shaker.hs -o $out/bin/chat-shaker
''
