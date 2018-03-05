{ mkDerivation, base, lens, parsers, stdenv, trifecta
, type-level-sets
}:
mkDerivation {
  pname = "pyfactor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base lens parsers trifecta type-level-sets
  ];
  executableHaskellDepends = [ base lens ];
  license = stdenv.lib.licenses.bsd3;
}
