{
  description = "Emacs solo nix flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, nixpkgs, home-manager, ... }: {
    homeManagerModules.emacs = { pkgs, ... }: {
      let hasEmacs = lib.any (pkg: lib.getName pkg == "emacs" || lib.hasPrefix "emacs-" (lib.getName pkg))
        (config.home.packages or []);
      in {
        # Add emacs only if not already present
        home.packages = lib.mkIf (!hasEmacs) [
          (lib.mkDefault pkgs.emacs)  # Default to `emacs`, but allow override
        ];

        home.file.".emacs.d/init.el" = {
          source = builtins.path { path = ./init.el; };
        };
        home.file.".emacs.d/early-init.el" = {
          source = builtins.path { path = ./early-init.el; };
        };
      };
    };
  };
}
