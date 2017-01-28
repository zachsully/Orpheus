{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hakaru, parsec, pretty, stdenv }:
      mkDerivation {
        pname = "orpheus";
        version = "0.0.1.0";
        src = ./.;
        libraryHaskellDepends = [ base hakaru parsec pretty ];
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
