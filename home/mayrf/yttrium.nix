{ config, inputs, pkgs, ... }:

{
  imports = [
    ../common
    ../features/general/impermanence.nix
    ../features/general/ensure-secrets-repo.nix
    ../features/general/ensure-private-config-repo.nix
    ../features/general/ensure-config-repo.nix
    ../features/cli/zsh.nix
    ../features/cli/fzf.nix
    ../features/cli/ai.nix
    ../features/cli/media.nix
    ../features/cli/development.nix
    ../features/cli/k8s.nix
    ../features/cli/leetcode.nix
    ../features/cli/yazi.nix
    ../features/cli/scripts
    ../features/cli/lf
    ../features/cli/git
    ../features/cli/syncthing.nix
    ../features/cli/sops.nix
    ../features/editor/emacs
    ../features/editor/nvim.nix
    ../features/editor/vscode.nix
    ../features/editor/zed.nix
    ../features/desktop/fonts.nix
    ../features/desktop/wayland.nix
    ../features/desktop/waybar.nix
    ../features/desktop/hyprland.nix
    ../features/desktop/gammastep.nix
    ../features/desktop/mako.nix
    ../features/desktop/wofi.nix
    ../features/desktop/nextcloud-client.nix
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
    ../features/terminal/foot.nix
    ../features/terminal/ghostty.nix
    inputs.dotfiles-private.outputs.homeManagerModules
    "${inputs.dotfiles-private}/home/desktop-apps.nix"
  ];

  colorscheme = inputs.nix-colors.colorschemes.woodland;

  features.impermanence.enable = true;
  features.impermanence.directories_cache = [
    ".local/share/docker"
  ];

  wayland.windowManager.hyprland = {
    settings = {
      monitor = [
        "DP-5,2560x1440@60,0x0,1"
        "DP-4,2560x1440@60,0x0,1"
        "DP-2,2560x1440@60,2560x0,1"
        "HDMI-A-1,1920x1080@120,auto-left,1"
        ",preferred,auto-right,1"
      ];
      workspace = [
        "1, monitor:DP-4, default:true"
        "2, monitor:DP-4"
        "3, monitor:DP-4"
        "4, monitor:DP-4"
        "5, monitor:DP-4"
        "1, monitor:DP-5, default:true"
        "2, monitor:DP-5"
        "3, monitor:DP-5"
        "4, monitor:DP-5"
        "5, monitor:DP-5"
        "6, monitor:DP-2"
        "7, monitor:DP-2"
        "8, monitor:DP-2"
        "9, monitor:DP-2"
        "10, monitor:DP-2"
        "F1, monitor:HDMI-A-1"
        "F2, monitor:HDMI-A-1"
        "F3, monitor:HDMI-A-1"
      ];
    };
  };

  home.packages = with pkgs; [
    urbit
    exercism
    vimgolf
    img2pdf
    gparted
    code-cursor
    rustdesk-flutter
    anydesk
    stable.teams-for-linux
  ];

  features.impermanence.directories = [
    ".cursor"
    ".config/Cursor"
  ];

  features.private.ssh.enable = true;
  features.private.personal.enable = true;
}
