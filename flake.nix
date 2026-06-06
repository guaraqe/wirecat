{
  description = "Development environment for wirecat";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    tricorder.url = "github:atelier-hub/tricorder";
  };

  outputs =
    {
      self,
      nixpkgs,
      tricorder,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forEachSystem =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          f {
            pkgs = import nixpkgs { inherit system; };
          }
        );
    in
    {
      devShells = forEachSystem (
        { pkgs }:
        let
          haskellPackages = pkgs.haskell.packages.ghc912;
        in
        {
          default = pkgs.mkShell {
            packages = [
              haskellPackages.ghc
              haskellPackages.cabal-install
              pkgs.ormolu
              pkgs.ghcid
              pkgs.graphviz
              pkgs.just
              pkgs.mermaid-cli
            ]
            ++ pkgs.lib.optional (
              tricorder.packages ? ${pkgs.system}
            ) tricorder.packages.${pkgs.system}.default;
          };
        }
      );

      formatter = forEachSystem ({ pkgs }: pkgs.nixfmt-rfc-style);
    };
}
