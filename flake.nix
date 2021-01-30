{
  description = "A very basic flake";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, haskell-nix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = haskell-nix.legacyPackages.${system};
        hsPkgs = pkgs.haskellPackages;

        haskellNix = pkgs.haskell-nix.cabalProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "too-many-logs-src";
            src = ./.;
          };
          compiler-nix-name = "ghc8103";
        };

        too-many-logs = haskellNix.too-many-logs.components.exes.tml;

        # TODO It's better to connect ghcid with this derivation directly.
        test-repl = pkgs.writeShellScriptBin "test-repl" ''
          ghcid -c 'cabal new-repl test:tml-test --flags=ghcid' -T ":main"
        '';

      in {
        packages.too-many-logs = too-many-logs;

        devShell = haskellNix.shellFor {
          packages = p: [ p.too-many-logs ];
          withHoogle = false;
          tools = {
            cabal = "3.2.0.0";
            haskell-language-server = "0.8.0";
            ghcid = "0.8.7";
          };
          nativeBuildInputs = [
            test-repl # Used for writing tests.
            hsPkgs.hpack
            haskellNix.too-many-logs.project.roots
          ];
          exactDeps = true;
        };

        defaultPackage = too-many-logs;
      });
}
