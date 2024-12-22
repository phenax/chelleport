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
          configurationFlags = [
            "--ghc-options=-O2"
          ];
          buildInputs = with pkgs; [
            xorg.libXtst
            xorg.libX11
            SDL2
            SDL2_ttf
          ];
        in {
          haskellProjects.default = {
            inherit projectRoot;

            packages = {};
            settings = {
              chelleport = {
                check = true;
                deadCodeElimination = true;
                staticLibraries = true;
                strip = true;
                custom = drv:
                  (pkgs.haskell.lib.compose.appendConfigureFlags configurationFlags drv).overrideAttrs (old: {
                    preBuild = ''
                      ${toString (map (f: ''cp -r ${f.source} ${f.target};'') otherFiles)}
                    '';
                  })
                ;
              };
            };

            devShell = {
              hlsCheck.enable = true;
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
            inherit buildInputs;

            LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";
          };
        };
    };
}
