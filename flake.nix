{
  description = "Minigraph development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Java 21 Temurin
        jdk = pkgs.temurin-bin-21;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Java
            jdk

            # Clojure
            clojure
            clj-kondo

            # JavaScript tools (needed for shadow-cljs)
            nodejs_20

            # Development tools
            git
            ripgrep

            # Optional but useful
            jq
            watchexec
          ];

          shellHook = ''
            export JAVA_HOME=${jdk}

            # Set up local paths for npm packages
            export PATH="$PWD/node_modules/.bin:$PATH"

            echo "🕸️ Minigraph Development Environment"
            echo ""
            echo "Available commands:"
            echo "  clojure -M:dev          - Start dev server"
            echo "  clojure -M:test/run     - Run tests"
            echo "  clj-kondo --lint src    - Run linter"
            echo "  nix flake update        - Update all dependencies"
            echo ""
            echo "Java:     $(java -version 2>&1 | head -n 1)"
            echo "Clojure:  $(clojure --version)"
            echo "Node:     $(node --version)"
          '';
        };
      });
}
