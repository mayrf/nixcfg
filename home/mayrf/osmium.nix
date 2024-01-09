{ config, lib, pkgs, inputs, outputs, ... }:

{

  home = {
    # username = lib.mkDefault "fmayr";
    # username = lib.mkDefault user;
    # homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "23.11";
  };
  imports = [
    # ./global-clean
    # ./features/lf
    # ./features/terminal/alacritty.nix
    # ./features/terminal/foot.nix
    # ./features/desktop/hyprland
    # ./features/editors/emacs/doom-emacs
    # ./features/editors/vscode.nix
    # ./features/postman.nix
    # ./features/editors/idea.nix
  ];
  # home.packages = with pkgs; [
  #   jetbrains.idea-community
  # ];
}
