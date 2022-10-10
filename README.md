# Setup notes

## Nix

You need an installation of the [Nix package
manager](https://nixos.org/download.html#download-nix), with `flakes`
enabled (see [the NixOS wiki](https://nixos.wiki/wiki/Flakes) for
instructions).

## VSCode

There are (at least) two alternative possibilities to get LSP
integration for Haskell working seamlessly:

### `direnv`

- Install [direnv](https://direnv.net/)
- Install the `mkhl.direnv` plugin in VSCode
- When opening the repository, the plugin will ask you to `Allow`
  usage of the `.envrc` file that already exists -> accept this
- Now you should see a prompt asking you to restart the extension ->
  also accept this
- Open the `MyLib.hs` file and change something; after a while, you
  should see some LSP feedback
  
### Nix environment selector

- Install the plugin `arrterian.nix-env-selector`
- After opening the repository, choose `Nix-Env: Select environment`
  from the command palette
- You should be prompted for a `Reload` -> accept this
- Open the `MyLib.hs` file and change something; after a while, you
  should see some LSP feedback
