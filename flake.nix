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
      # The transcode module (milestone 3b) shells out to ffmpeg at runtime,
      # so the produced binary is wrapped to have ffmpeg on its PATH.
      mkHaskellPackages =
        pkgs:
        pkgs.haskellPackages.extend (
          hself: hsuper: {
            functional-media-server =
              (hsuper.callCabal2nix "functional-media-server" ./. { }).overrideAttrs (old: {
                nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
                buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.ffmpeg ];
                postInstall = (old.postInstall or "") + ''
                  wrapProgram $out/bin/fms \
                    --prefix PATH : ${pkgs.ffmpeg}/bin
                '';
              });
          }
        );
    in
    {
      packages = forAllSystems (pkgs:
        let
          fmsPkg = (mkHaskellPackages pkgs).functional-media-server;
        in
        {
          default = fmsPkg;

          # Container image (docs/todo.org section 16, milestone 13/16):
          # the wrapped fms binary (ffmpeg already on its PATH via the
          # wrapProgram in mkHaskellPackages), plus ffmpeg/coreutils/bash.
          # /bin/fms is guaranteed by extraCommands regardless of how
          # dockerTools merges the contents' bin directories.
          docker = pkgs.dockerTools.buildLayeredImage {
            name = "functional-media-server";
            tag = "latest";
            contents = [
              fmsPkg
              pkgs.ffmpeg
              pkgs.coreutils
              pkgs.bash
            ];
            extraCommands = ''
              mkdir -p bin var/lib/fms media
              if [ ! -e bin/fms ]; then
                ln -s ${fmsPkg}/bin/fms bin/fms
              fi
            '';
            config = {
              Cmd = [ "/bin/fms" ];
              Env = [
                "PATH=/bin"
                "MEDIA_LIBRARY_PATH=/media"
                "MEDIA_SERVER_PORT=8096"
                "FMS_STATE_DIR=/var/lib/fms"
              ];
              ExposedPorts = { "8096/tcp" = { }; };
              WorkingDir = "/var/lib/fms";
            };
          };
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
              ffmpeg
            ];
          };
      });
    };
}
