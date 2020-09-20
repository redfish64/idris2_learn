{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc882" }:
let
  pkgs = import <nixpkgs> {};
in
with pkgs;
stdenv.mkDerivation {
  name = "idris-learn2-env-0";

  #stdenv.mkDerivation will setup the path for these build inputs here
  buildInputs = [ myidris2 chez
    ];
  shellHook = ''
    '';
}  
