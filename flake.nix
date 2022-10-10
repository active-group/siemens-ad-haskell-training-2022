{
  description = "Playground setup for the Haskell training";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ];
          config.allowUnfree = true;
        };
      in {
        devShells.default =
          pkgs.mkShell { nativeBuildInputs = with pkgs; [ cabal-install ]; };
      });
}
