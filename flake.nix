{
  description = "A very basic flake";

  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hsPkgs = pkgs.haskellPackages;

        too-many-logs = hsPkgs.callCabal2nix "too-many-logs" ./. { };

      in {
        packages.too-many-logs = too-many-logs;

        devShell = hsPkgs.shellFor {
          packages = p: [ too-many-logs ];
          withHoogle = true;
          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.hpack
            hsPkgs.ghcid
            hsPkgs.haskell-language-server
          ];
        };

        defaultPackage = too-many-logs;
      });
}
