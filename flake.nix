{
    inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
    inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    inputs.flake-utils.url = "github:numtide/flake-utils";

    outputs = { self, nixpkgs, flake-utils, haskellNix }:
        flake-utils.lib.eachDefaultSystem (system:
        let
            overlays = [ haskellNix.overlay
                (final: prev: {
                hsPkgs =
                    final.haskell-nix.project' {
                        src = ./.;
                        shell.tools = {
                            cabal = "latest";
                            stack = "2.9.1";
#                            haskell-language-server = "latest";
#                            hlint = "latest";
#                            ormolu = "latest";
                        };
                    };
                })
            ];
            pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
            flake = pkgs.hsPkgs.flake { };
        in
            flake // {
                packages.default = flake.packages."blog:exe:blog-exe";
            }
        );
}
