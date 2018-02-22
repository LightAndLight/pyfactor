{ mkDerivation, base, lens, stdenv, type-level-sets }:
mkDerivation {
  pname = "pyfactor";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens type-level-sets ];
  license = stdenv.lib.licenses.bsd3;
}
