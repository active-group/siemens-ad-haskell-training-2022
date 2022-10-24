{
  description = "Playground setup for the Haskell training";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    polysemySrc = {
      url = "github:polysemy-research/polysemy";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              haskellPackages = prev.haskellPackages.override (old: {
                overrides =
                  final.lib.composeExtensions (old.overrides or (_: _: { }))
                  (hfinal: hprev: {
                    haskell-training = hfinal.callCabal2nix "haskell-training"
                      (final.lib.cleanSource ./.) { };
                    # Use the newest version of polysemy.
                    polysemy = hfinal.callCabal2nix "polysemy"
                      self.inputs.polysemySrc { };
                    polysemy-plugin = hfinal.callCabal2nix "polysemy"
                      (self.inputs.polysemySrc + /polysemy-plugin) { };
                  });
              });
            })
          ];
          config.allowUnfree = true;
        };
      in {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.haskell-training ];
          nativeBuildInputs = with pkgs; [
            cabal-install
            haskell-language-server
            haskellPackages.fourmolu
          ];
        };
      });
}
