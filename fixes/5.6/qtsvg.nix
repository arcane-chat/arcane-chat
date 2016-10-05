{ qtSubmodule, qtbase }:

qtSubmodule {
  name = "qtsvg";
  qtInputs = [ qtbase ];
  patches = ./../qtsvg-epsilon.patch;
}
