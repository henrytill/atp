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
        devPackagesQuery = {
          ocaml-lsp-server = "*";
          ocamlformat = "*";
        };
        query = devPackagesQuery // {
          ocaml-base-compiler = "5.3.0";
        };
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        scope = on.buildOpamProject {
          resolveArgs.with-test = true;
          resolveArgs.with-doc = true;
        } package ./. query;
        overlay = final: prev: {
          ${package} = prev.${package}.overrideAttrs (as: {
            postBuild = ''
              dune build @doc
            '';
            postInstall = ''
              mkdir -p $out/share/doc/${package}
              cp -r _build/default/_doc/_html $out/share/doc/${package}/html
            '';
          });
        };
        legacyPackages = scope.overrideScope overlay;
        devPackages = builtins.attrValues (
          pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) legacyPackages
        );
        atpShell = pkgs.mkShell {
          inputsFrom = with legacyPackages; [
            atp
          ];
          packages = devPackages ++ [ ];
        };
      in
      {
        inherit legacyPackages;
        packages.default = self.legacyPackages.${system}.${package};
        devShells.default = atpShell;
      }
    );
}
