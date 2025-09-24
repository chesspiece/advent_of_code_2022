{
  description = "Haskell dev shells with selectable GHC for VS Code";

  inputs = {
    # Pick a channel you like (stable shown). You can switch to "nixpkgs-unstable" if you prefer.
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Helper: build a dev shell for a given haskell package set (i.e., a specific GHC)
        mkShellFor = comp: pkgs.mkShell {
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
          nativeBuildInputs = with pkgs; [ pkg-config ];
          buildInputs = with pkgs; [
            # zlib
            # openssl
            # sqlite
          ];

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
      in
      {
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
      });
}