{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];

      perSystem = { self', pkgs, lib, config, ... }:
        let
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./bin
              ./src
              ./specs
              ./chelleport.cabal
            ];
          });
          otherFiles = [
            { source = ./static; target = "static"; }
          ];
        in {
          haskellProjects.default = {
            inherit projectRoot;

            packages = {};
            settings = {
              chelleport = {
                deadCodeElimination = true;
                staticLibraries = true;
                # extraBuildFlags = ["+release"];
                strip = true;
                custom = drv: drv.overrideAttrs(old: {
                  preBuild = ''
                    ${toString (map (f: ''cp -r ${f.source} ${f.target};'') otherFiles)}
                  '';
                });
              };
            };

            devShell = {
              # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
              hlsCheck.enable = false;
            };

            autoWire = [ "packages" "apps" "checks" ];
          };

          packages.default = self'.packages.chelleport;
          apps.default = self'.apps.chelleport;

          devShells.default = pkgs.mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
            packages = with pkgs; [
              just
              nodemon
            ];
          };
        };
    };
}
