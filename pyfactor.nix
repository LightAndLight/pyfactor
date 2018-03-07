{ mkDerivation, base, hedgehog, lens, mtl, parsers, process, stdenv
, trifecta, type-level-sets
}:
mkDerivation {
  pname = "pyfactor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base lens mtl parsers trifecta type-level-sets
  ];
  executableHaskellDepends = [ base lens ];
  testHaskellDepends = [ base hedgehog lens process ];
  license = stdenv.lib.licenses.bsd3;
}
