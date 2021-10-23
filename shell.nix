{pkgs ? import <nixpkgs> {} }:
let
  base = pkgs.callPackage ./default.nix { src = ./.; };
in
base.overrideAttrs (old: {
  buildInputs = pkgs.lib.concat old.buildInputs [pkgs.haskell-language-server];
})
