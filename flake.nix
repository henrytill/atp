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
      makeAtp =
        pkgs:
        {
          compiler ? "ghc948",
          doCheck ? true,
        }:
        let
          call = compiler: pkgs.haskell.packages.${compiler}.callCabal2nixWithOptions;
          flags = "";
          src = builtins.path {
            path = ./.;
            name = "atp-src";
          };
          atp_ = call compiler "atp" src flags { };
        in
        pkgs.haskell.lib.overrideCabal atp_ (_: {
          inherit doCheck;
          isExecutable = true;
          isLibrary = true;
          doHaddock = false;
        });
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.atp = makeAtp pkgs { };
        packages.default = self.packages.${system}.atp;
      }
    );
}
