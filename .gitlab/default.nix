let sources = import ./nix/sources.nix; in

{ nixpkgs ? (import sources.nixpkgs {}) }:

with nixpkgs;
let
  gen_ci = nixpkgs.callPackage ./gen_ci {};

in
  symlinkJoin {
    name = "gen_ci";
    preferLocalBuild = true;
    paths = [
      gen_ci
    ];
  }
