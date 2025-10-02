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
            # nativeBuildInputs = with pkgs; [ pkg-config ];
            # buildInputs = with pkgs; [
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
          program = "${drv}/bin/adventofcode2022";
        };
      in
      {
        packages = {
          project-ghc94 = mkProject ghc94;
          project-ghc96 = mkProject ghc96;
          project-ghc98 = mkProject ghc98;

          # Default build target (change if you prefer a different default GHC)
          default = mkProject ghc96;
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