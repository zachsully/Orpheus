{ mkDerivation, alex, array, base, hakaru, happy, mtl, mwc-random
, parsec, pretty, stdenv, vector
}:
mkDerivation {
  pname = "orpheus";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base hakaru mtl mwc-random parsec pretty vector
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base hakaru mwc-random pretty ];
  homepage = "zachsully.com";
  description = "Music analysis";
  license = stdenv.lib.licenses.bsd3;
}
