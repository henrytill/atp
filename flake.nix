{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.opam-repository.follows = "opam-repository";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      follows = "opam-nix/flake-utils";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    {
      self,
      flake-utils,
      opam-nix,
      nixpkgs,
      ...
    }@inputs:
    let
      package = "atp";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        scope = on.buildOpamProject {
          resolveArgs.with-test = true;
          resolveArgs.with-doc = true;
        } package ./. { ocaml-base-compiler = "5.3.0"; };
        overlay = final: prev: { ${package} = prev.${package}.overrideAttrs (as: { }); };
      in
      {
        legacyPackages = scope.overrideScope overlay;
        packages.default = self.legacyPackages.${system}.${package};
      }
    );
}
