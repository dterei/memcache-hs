{ pkgs ? import <nixpkgs> {} }:
let
  hsPkgs = pkgs.haskellPackages;
  project = pkgs.callPackage ./default.nix {};
in pkgs.mkShell {
    name = "cabal-shell";
    inputsFrom = [ project.env ];
    buildInputs = [
      hsPkgs.cabal-install
    ];
}
