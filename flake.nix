{
  description = "Haskell dev shells with selectable GHC for VS Code";

  inputs = {
    # Pick a channel you like (stable shown). You can switch to "nixpkgs-unstable" if you prefer.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        exeName = "adventofcode2022";

        # Helper: build a dev shell for a given haskell package set (i.e., a specific GHC)
        mkShellFor =
          comp:
          pkgs.mkShell {
            # Tools come from the SAME package set as GHC, so HLS exactly matches GHC.
            packages = with comp; [
              ghc
              cabal-install
              haskell-language-server
              hlint
              hoogle
              fourmolu
            ];

            # Common native deps people often need; tweak for your libs
            nativeBuildInputs = with pkgs; [
              pkg-config
              bash
              zsh
              fish
            ];
            #buildInputs = with pkgs; [
            #   # zlib
            #   # openssl
            #   # sqlite
            # ];

            # Nice to see which GHC you landed in
            shellHook = ''
              echo ">>> Haskell dev shell"
              echo ">>> $(ghc --version)"
            '';
          };

        ghc94 = pkgs.haskell.packages.ghc94;
        ghc96 = pkgs.haskell.packages.ghc96;
        ghc98 = pkgs.haskell.packages.ghc98;
        # Add more if you like, e.g.:
        # ghc910 = pkgs.haskell.packages.ghc910;  # if supported on your nixpkgs revision

        # Reproducible build for a given GHC
        mkProject = ghc: ghc.callCabal2nix "project" ./. { };

        # Helper to make a nix run app for a given drv+exe
        mkAppFor = drv: {
          type = "app";
          program = "${drv}/bin/${exeName}";
        };

        mkImage =
          { name, drv }:
          pkgs.dockerTools.buildLayeredImage {
            inherit name;
            tag = "latest";
            contents = [
              drv
              pkgs.cacert
              pkgs.bashInteractive # useful for debugging containers
              pkgs.coreutils # minimal tools
            ];
            config = {
              # Run the app by default; change or add args if needed
              Cmd = [ "${drv}/bin/${exeName}" ];
              # Basic environment (locale optional; uncomment if you need it)
              # Env = [
              #   "LANG=C.UTF-8"
              #   "LC_ALL=C.UTF-8"
              #   "LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive"
              # ];
              WorkingDir = "/work";
            };
            # Optional: add a writable /work
            extraCommands = ''
              mkdir -p work
            '';
          };

        # Build derivations for each compiler
        project94 = mkProject ghc94;
        project96 = mkProject ghc96;
        project98 = mkProject ghc98;

        image94 = mkImage {
          name = "${exeName}-ghc94";
          drv = project94;
        };
        image96 = mkImage {
          name = "${exeName}-ghc96";
          drv = project96;
        };
        image98 = mkImage {
          name = "${exeName}-ghc98";
          drv = project98;
        };
      in
      {
        packages = {
          project-ghc94 = project94;
          project-ghc96 = project96;
          project-ghc98 = project98;

          # Default build target
          default = project96;

          # Docker/OCI images
          dockerImage-ghc94 = image94;
          dockerImage-ghc96 = image96;
          dockerImage-ghc98 = image98;

          # Default docker build target
          dockerImage = image96;
        };

        # Build checks (handy for CI and `nix flake check`)
        checks = {
          project-ghc94 = self.packages.${system}.project-ghc94;
          project-ghc96 = self.packages.${system}.project-ghc96;
          project-ghc98 = self.packages.${system}.project-ghc98;
        };

        ############
        ## nix run ##
        ############
        # Run the built executable directly:
        # - nix run                 (uses apps.default → ghc96)
        # - nix run .#run-ghc94
        # - nix run .#run-ghc98
        apps = {
          default = mkAppFor self.packages.${system}.project-ghc96;
          run-ghc94 = mkAppFor self.packages.${system}.project-ghc94;
          run-ghc96 = mkAppFor self.packages.${system}.project-ghc96;
          run-ghc98 = mkAppFor self.packages.${system}.project-ghc98;
        };

        devShells = {
          # Default shell (adjust to your preference)
          default = mkShellFor ghc96;

          # Versioned shells selectable via `nix develop .#ghc96`, etc.
          ghc94 = mkShellFor ghc94;
          ghc96 = mkShellFor ghc96;
          ghc98 = mkShellFor ghc98;
        };

        # `nix fmt` with the modern formatter
        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}