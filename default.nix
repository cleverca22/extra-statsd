let
  pkgs = import <nixpkgs> {};
  overrides =  self: super: {
    datadog = pkgs.haskell.lib.dontCheck super.datadog;
  };
  hspkgs = pkgs.haskellPackages.override { inherit overrides; };
in 
  hspkgs.callCabal2nix "extra-statsd" ./. {}
