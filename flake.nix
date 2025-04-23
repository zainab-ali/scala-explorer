{
  description = "Dev shell";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-24.11;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem
    (
      system: let
        nodeOverlay = f: p: {
          nodejs = p.nodejs-18_x;
        };
        scalaOverlay = f: p: {
          scala = p.scala.bare.overrideAttrs(old: {
            version = "3.6.4";
            src = builtins.fetchurl {
              inherit (old.src) url;
              sha256 = "sha256:19f413cmbw9hrpp67ri48c7xvds1gyp3dvww05r2554yysmnkhi3";
            };
          });
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [nodeOverlay scalaOverlay];
        };
      in {
        formatter = nixpkgs.legacyPackages.${system}.alejandra;

        devShells.default = pkgs.mkShell {
          name = "shell";
          buildInputs = with pkgs; [
            # Why is the scala version still not overridden?
            scala
            nodejs
          ];
        };
      }
    );
}
