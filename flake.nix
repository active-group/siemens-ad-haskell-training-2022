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
        demo-port = "8999";
        demo-put =
          pkgs.writeShellScriptBin "demo-put" ''
            if [ ''${#} -ne 2 ]; then
              echo "demo-put <aspect-name> <ribbon-id>"
            else
              curl -i -X PUT -d "\"''${2}\"" \
                   -H "Content-Type: application/json" \
                   localhost:${demo-port}/''${1}/color-ribbon
            fi
          '';
        demo-del =
          pkgs.writeShellScriptBin "demo-del" ''
            if [ ''${#} -ne 1 ]; then
              echo "demo-del <aspect-name>"
            else
              curl -i -X DELETE \
                   localhost:${demo-port}/''${1}/color-ribbon
            fi
          '';
      in {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.haskell-training ];
          nativeBuildInputs = with pkgs; [
            cabal-install
            haskell-language-server
            haskellPackages.fourmolu
            curl
            demo-put
            demo-del
          ];
        };
      });
}
