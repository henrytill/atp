{
  description = "Handbook of Practical Logic and Automated Reasoning";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    let
      ghcName = "ghc9103";
      overlay = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcName} = prev.haskell.packages.${ghcName}.override {
              overrides = hfinal: hprev: {
                atp = hfinal.callCabal2nix "atp" self { };
              };
            };
          };
        };
      };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in
      {
        packages.atp = pkgs.haskell.packages.${ghcName}.atp;
        packages.default = self.packages.${system}.atp;
        devShells.default = pkgs.haskell.packages.${ghcName}.shellFor {
          packages = hpkgs: [
            hpkgs.atp
          ];
          withHoogle = true;
          nativeBuildInputs = with pkgs; [
            cabal-install
            haskell.packages.${ghcName}.fourmolu
            haskell.packages.${ghcName}.ghc-tags
            haskell.packages.${ghcName}.hlint
            haskell.packages.${ghcName}.weeder
          ];
        };
      }
    );
}
