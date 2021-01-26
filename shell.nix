{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
in
mkShell {
  buildInputs = [
    elmPackages.elm
    elmPackages.elm-live
    nodejs-12_x
    nodePackages.webpack
    nodePackages.webpack-cli
  ];
}
