{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alex, array, base, hakaru, happy, mtl
      , mwc-random, parsec, pretty, stdenv, vector
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
