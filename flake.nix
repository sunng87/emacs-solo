{
  description = "Emacs solo nix flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, nixpkgs, home-manager, ... }: {
    homeManagerModules.emacs = { pkgs, lib, config, ... }: {
      # This will work alongside home-manager's programs.emacs
      # Just providing the config files
      home.file.".emacs.d/init.el" = {
        source = builtins.path { path = ./init.el; };
      };
      home.file.".emacs.d/early-init.el" = {
        source = builtins.path { path = ./early-init.el; };
      };
    };
  };
}
