{ runCommand, haskellPackages }:

let paths = p: with p; [ shake shake-language-c ];
    ghc = haskellPackages.ghcWithPackages paths;
in runCommand "chat-shaker" { buildInputs = [ ghc ]; } ''
    mkdir -pv $out/bin
    cp -vi ${./chat-shaker.hs} chat-shaker.hs
    ghc chat-shaker.hs -o $out/bin/chat-shaker
''
