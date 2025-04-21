{
  description = "emacs-solo: the standalone emacs setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, nixpkgs, home-manager, ... }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    homeManagerModules.default = { pkgs, lib, config, ... }:
    let
      cfg = config.programs.emacs-solo;
    in {
      options.programs.emacs-solo = {
        enable = lib.mkEnableOption "emacs-solo configuration";

        package = lib.mkOption {
          type = lib.types.package;
          default = pkgs.emacs;
          description = "The Emacs package to use";
        };
      };

      config = lib.mkIf cfg.enable {
        home.file.".emacs-solo/init.el".source = ./init.el;
        home.file.".emacs-solo/early-init.el".source = ./early-init.el;

        home.packages = [
          (pkgs.writeShellScriptBin "e" ''
            exec ${cfg.package}/bin/emacs --init-directory="$HOME/.emacs-solo" "$@"
          '')
        ];
      };
    };
  };
}
