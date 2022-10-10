# NOTE: We're not really using old-style (stable) Nix projects anymore, but
# have since fully committed to using flakes.  This is a compatibility
# shell that only exists to make working with certain tools (like
# VSCode's nix-environment-selector plugin) possible.

let
  flake-compat = let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  in fetchTarball {
    url =
      "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash;
  };
in (import flake-compat { src = ./.; }).shellNix
