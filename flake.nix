{
  description = "Don't Worry Be Happy. Parsing Examples with Happy and Alex";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-21.05;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    haskell-language-server = {
      url = github:haskell/haskell-language-server/1.4.0-hackage;
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, haskell-language-server, flake-utils}:
    let
      overlays = [
        haskell-language-server.overlay
      ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = _: [];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.happy
            pkgs.haskellPackages.alex
          ];
        };
        defaultPackage =
          (pkgs.haskellPackages.callCabal2nix "dont-worry-be-happy" ./. {}).overrideAttrs (prev: {
            nativeBuildInputs = prev.nativeBuildInputs ++ [
              pkgs.haskellPackages.happy
              pkgs.haskellPackages.alex
            ];
          });
      }) // { inherit overlays; };
}
