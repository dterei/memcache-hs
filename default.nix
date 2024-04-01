{ pkgs ? import <nixpkgs> { } }:
let
  hsPkgs = pkgs.haskellPackages;

  src = builtins.path {
    name = "memcache-hs-src";
    path = ./.;
    filter = path: type:
      let
        basePath = builtins.baseNameOf path;
      in
      basePath != "dist-newstyle"
    ;
  };

  project = hsPkgs.callCabal2nix "memcache-hs" src;
in
hsPkgs.callPackage project { }
