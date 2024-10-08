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
        scope = on.buildOpamProject { resolveArgs.with-test = true; } package ./. {
          ocaml-base-compiler = "4.14.2";
        };
        overlay = final: prev: {
          atp = prev.atp.overrideAttrs (as: {
            nativeBuildInputs = as.nativeBuildInputs ++ [
              (pkgs.python3.withPackages (ps: [ ps.cram ]))
              pkgs.makeWrapper
            ];
            dontStrip = true;
            postInstall = ''
              wrapProgram $out/bin/main.byte --prefix CAML_LD_LIBRARY_PATH : "$CAML_LD_LIBRARY_PATH"
            '';
          });
        };
      in
      {
        legacyPackages = scope.overrideScope' overlay;
        packages.default = self.legacyPackages.${system}.${package};
      }
    );
}
