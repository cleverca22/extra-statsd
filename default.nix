{ pkgs ? import <nixpkgs> {} }:
let
  overrides =  self: super: {
    datadog = pkgs.haskell.lib.dontCheck super.datadog;
  };
  hspkgs = pkgs.haskellPackages.override { inherit overrides; };
  pkg = hspkgs.callCabal2nix "extra-statsd" ./. {};
in {
  normal = pkg;
  static = pkgs.haskell.lib.justStaticExecutables pkg;
}
