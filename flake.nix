{
  description = "aiken-codegen - Haskell DSL for generating Aiken source";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          overlays = [ haskellNix.overlay ];
          inherit system;
        };
        project = import ./nix/project.nix { inherit pkgs; };
      in {
        packages = project.packages // {
          default = project.packages."aiken-codegen:lib:aiken-codegen";
          unit-tests = project.packages."aiken-codegen:test:golden-tests";
        };
        inherit (project) devShells;
      });
}
