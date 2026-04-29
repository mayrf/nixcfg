{ inputs, ... }:

{
  imports = [
    ../common
    ../features/general/impermanence.nix
    ../features/general/ensure-secrets-repo.nix
    ../features/general/ensure-private-config-repo.nix
    ../features/general/ensure-config-repo.nix
    ../features/cli/zsh.nix
    ../features/cli/fzf.nix
    ../features/cli/development.nix
    ../features/cli/k8s.nix
    ../features/cli/scripts
    ../features/cli/yazi.nix
    ../features/cli/lf
    ../features/cli/git
    ../features/cli/sops.nix
    ../features/cli/syncthing.nix
    ../features/editor/nvim.nix
    ../features/editor/emacs
    ../features/editor/vscode.nix
    ../features/desktop/fonts.nix
    ../features/desktop/wayland.nix
    ../features/desktop/waybar.nix
    ../features/desktop/hyprland.nix
    ../features/desktop/gammastep.nix
    ../features/desktop/mako.nix
    ../features/desktop/wofi.nix
    ../features/desktop/nextcloud-client.nix
    ../features/desktop/opencloud-client.nix
    ../features/desktop/virtualisation.nix
    ../features/desktop/postman.nix
    ../features/desktop/librewolf.nix
    ../features/desktop/gpg.nix
    ../features/desktop/zathura.nix
    ../features/desktop/learning.nix
    ../features/desktop/media.nix
    ../features/desktop/social.nix
    ../features/desktop/productivity.nix
    ../features/desktop/zen-browser.nix
    ../features/terminal/alacritty.nix
    ../features/terminal/ghostty.nix
    inputs.dotfiles-private.outputs.homeManagerModules
    "${inputs.dotfiles-private}/home/desktop-apps.nix"
  ];

  features.impermanence.enable = true;

  colorscheme = inputs.nix-colors.colorschemes.woodland;

  wayland.windowManager.hyprland = {
    settings = {
      monitor = [ "LVDS-1,1366x768@60,0x0,1" ];
      workspace = [
        "1, monitor:LVDS-1, default:true"
        "2, monitor:LVDS-1"
        "3, monitor:LVDS-1"
        "4, monitor:LVDS-1"
        "5, monitor:LVDS-1"
        "6, monitor:LVDS-1"
        "7, monitor:LVDS-1"
      ];
    };
  };

  features.private.ssh.enable = true;
}
