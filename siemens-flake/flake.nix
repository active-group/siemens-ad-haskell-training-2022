{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: 
    let system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system; };
    in {

    # special output key names:
    # packages
    # devShells
    # checks
    # apps
    # templates
    # nixosConfiguration
    # ...

    # the kind of the output determines what CLI commands you
    # can or should use with/for it

    # packages.${system}.<my-package-name> = <my-package-definition>

    # special:
    # packages.${system}.default = <my-package-definition>
    # -> you can skip the package name in CLI commands

    # you can _build_ packages:
    # nix build .#<output-id>, like nix build .#hello-siemens-package
    #
    # sometimes you can run them as well:
    # nix run .#<id> -- -some -arguments

    packages.x86_64-linux.cowsay = pkgs.cowsay;
    # packages.aarch64-darwin.cowsay = nixpkgs.legacyPackages.aarch64-darwin.cowsay;

    packages.x86_64-linux.default = self.packages.x86_64-linux.cowsay;

    packages.x86_64-linux.hello-siemens-package = 
      pkgs.callPackage ./hello-siemens.nix {};

    # an output for a minimal Docker image containing JUST our application
    packages.x86_64-linux.hello-siemens-docker = 
        pkgs.dockerTools.buildImage {
          name = "hello-siemens-image";
          tag = "latest";
          # copy the following packages to the root file system, so
          # that they're accessible anywhere within the image
          copyToRoot = [self.packages.x86_64-linux.hello-siemens-package];
          config = {
            Cmd = [ "hello-siemens-package" ];
          };
        };

    # development environments:

    # develop a package: nix develop .#<id>

    devShells.x86_64-linux.default = pkgs.mkShell {
      buildInputs = with pkgs; [ cowsay nodejs clojure ];
    };
  };
}
