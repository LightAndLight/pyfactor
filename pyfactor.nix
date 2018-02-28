{ mkDerivation, base, lens, parsers, stdenv, type-level-sets }:
mkDerivation {
  pname = "pyfactor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base lens parsers type-level-sets ];
  executableHaskellDepends = [ base lens ];
  license = stdenv.lib.licenses.bsd3;
}
