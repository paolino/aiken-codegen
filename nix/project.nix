{ pkgs }:
let
  indexState = "2026-02-01T00:00:00Z";
  project = pkgs.haskell-nix.cabalProject' {
    src = ../.;
    compiler-nix-name = "ghc984";
    index-state = indexState;
    shell = {
      tools = {
        cabal = { index-state = indexState; };
        cabal-fmt = { index-state = indexState; };
        haskell-language-server = { index-state = indexState; };
        hoogle = { index-state = indexState; };
        fourmolu = { index-state = indexState; };
        hlint = { index-state = indexState; };
      };
      buildInputs = with pkgs; [ just nixfmt-classic ];
    };
  };
  flake = project.flake { };
in {
  packages = flake.packages;
  devShells.default = project.shell;
}
