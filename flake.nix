{
  description = "Emacs Solo: Always-overwrite config";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs, ... }: let
    system = "x86_64-linux";  # Adjust as needed
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    packages.${system} = {
      emacs-solo = pkgs.stdenv.mkDerivation {
        name = "emacs-solo";
        src = ./.;

        installPhase = ''
          mkdir -p $out/bin
          cat > $out/bin/emacs-solo <<EOF
          #!/bin/sh
          # Force-copy latest init.el to ~/.emacs-solo/
          mkdir -p "\$HOME/.emacs-solo"
          cp ${./init.el} "\$HOME/.emacs-solo/init.el"
          cp ${./early-init.el} "\$HOME/.emacs-solo/early-init.el"
          # Launch
          exec ${pkgs.emacs}/bin/emacs --init-directory="\$HOME/.emacs-solo" "\$@"
          EOF
          chmod +x $out/bin/emacs-solo
        '';
      };
      default = self.packages.${system}.emacs-solo;
    };
  };
}
