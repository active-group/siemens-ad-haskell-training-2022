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
          overlays = [
            (final: prev: {
              haskellPackages = prev.haskellPackages.override (old: {
                overrides =
                  final.lib.composeExtensions (old.overrides or (_: _: { }))
                  (hfinal: hprev: {
                    haskell-training = hfinal.callCabal2nix "haskell-training"
                      (final.lib.cleanSource ./.) { };
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
          ];
        };
      });
}
