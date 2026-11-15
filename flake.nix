{
  description = "functional-media-server - a Jellyfin-like media server written in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
      # The cabal package, translated to a Nix expression by cabal2nix.
      mkHaskellPackages =
        pkgs:
        pkgs.haskellPackages.extend (
          hself: hsuper: {
            functional-media-server = hsuper.callCabal2nix "functional-media-server" ./. { };
          }
        );
    in
    {
      packages = forAllSystems (pkgs: {
        default = (mkHaskellPackages pkgs).functional-media-server;
      });

      devShells = forAllSystems (pkgs: {
        default =
          let
            hpkgs = mkHaskellPackages pkgs;
          in
          hpkgs.shellFor {
            packages = p: [ p.functional-media-server ];
            buildInputs = with pkgs; [
              cabal-install
              cabal2nix
              ghc
              haskell-language-server
            ];
          };
      });
    };
}
