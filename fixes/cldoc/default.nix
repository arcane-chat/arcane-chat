{ fetchFromGitHub, python27Packages, clang }:

python27Packages.buildPythonApplication rec {
  name = "cldoc";

  src = fetchFromGitHub {
    owner  = "jessevdk";
    repo   = "cldoc";
    rev    = "f1b21dbabab40a4898d7dd5caaf145420f50691d";
    sha256 = "126p0ryzr9dpbpigpvs7bfl2fzyaqvsp9yr2022kf5p974cwkafw";
  };

  propagatedBuildInputs = with python27Packages; [
    pyparsing1
    clang.cc.lib
  ];
}
