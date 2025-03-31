{
  description = "Barebones emacs-solo for Home Manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs: let
    system = "x86_64-linux";  # Change if needed
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    # Home Manager module
    homeManagerModules.default = { config, pkgs, ... }: {
      home.packages = [
        (pkgs.writeShellScriptBin "emacs-solo" ''
          # Force-copy configs to ~/.emacs-solo/
          mkdir -p "$HOME/.emacs-solo"
          cat ${./init.el} > "$HOME/.emacs-solo/init.el"
          cat ${./early-init.el} > "$HOME/.emacs-solo/early-init.el"
          # Launch
          exec ${pkgs.emacs}/bin/emacs --init-directory="$HOME/.emacs-solo" "$@"
        '')
      ];
    };
  };
}
