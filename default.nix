{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "mmsyn7ukr-common";
  version = "0.3.1.0";
  src = ./.;
  libraryHaskellDepends = with pkgs.haskellPackages; [ base directory end-of-exe process ];
  homepage = "https://hackage.haskell.org/package/mmsyn7ukr-common";
  description = "Some common for mmsyn7ukr and mmsyn7ukr-array functionality using SoX";
  license = pkgs.lib.licenses.mit;
}
