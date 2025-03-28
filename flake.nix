{
  description = "Emacs solo nix flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, nixpkgs, home-manager, ... }: {
    homeManagerModules.emacs = { pkgs, ... }: {
      home.packages = [ pkgs.emacs ];  # Ensure Emacs is installed
      home.file.".emacs.d/init.el" = {
        source = "./init.el";
      };
      home.file.".emacs.d/early-init.el" = {
        source = "./early-init.el";
      };
    };
  };
}
