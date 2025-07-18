{ config, lib, pkgs, inputs, outputs, ... }:

{
  imports = [
    ../common
    ../features
    ../features/cli
    ../features/desktop
    ../features/terminal
    ../features/editor
    inputs.dotfiles-private.outputs.homeManagerModules
    "${inputs.dotfiles-private}/home/desktop-apps.nix"
  ];

  colorscheme = inputs.nix-colors.colorschemes.woodland;
  features = {
    ensure-secrets-repo.enable = true;
    ensure-private-config-repo.enable = true;
    ensure-config-repo.enable = true;
    impermanence.enable = true;
    impermanence.directories_no_bak = [ ".local/share/docker" ];
    # podman.enable = true;
    cli = {
      zsh.enable = true;
      fzf.enable = true;
      ai.enable = true;
      media.enable = true;
      development.enable = true;
      k8s.enable = true;
      yazi.enable = true;
      lf.enable = true;
      git.enable = true;
      # If this is not activated, I get a weird error: This is probably because something tries to access sops while it is not configured. This error should be avoided.
      # Failed assertions:
      # - mayrf profile: No key source configured for sops. Either set services.openssh.enable or set sops.age.keyFile or sops.gnupg.home or sops.gnupg.qubes-split-gpg.enable
      sops.enable = true;
    };
    editor = {
      nvim.enable = true;
      emacs.enable = true;
    };
    desktop = {
      wayland.enable = true;
      # email.enable = true;
      waybar.enable = true;
      hyprland.enable = true;
      gammastep.enable = true;
      mako.enable = true;
      wofi.enable = true;
      nextcloud-client.enable = true;
      virtualisation.enable = true;
      postman.enable = true;
      librewolf.enable = true;
      gpg.enable = true;
      zathura.enable = true;
      learning.enable = true;
      media.enable = true;
      social.enable = true;
      productivity.enable = true;
      zen-browser.enable = true;
    };
    terminal = {
      alacritty.enable = true;
      foot.enable = true;
      ghostty.enable = true;
    };
    private = { ssh.enable = true; };
  };

  wayland.windowManager.hyprland = {
    settings = {
      monitor = [
        "HDMI-A-1,2560x1440@60,0x0,1"
        "DP-2,2560x1440@60,2560x0,1"
        # "DP-2,2560x1440@60,2560x-560,1,transform,3"
      ];
      workspace = [
        "1, monitor:HDMI-A-1, default:true"
        "2, monitor:HDMI-A-1"
        "3, monitor:HDMI-A-1"
        "4, monitor:HDMI-A-1"
        "5, monitor:HDMI-A-1"
        "6, monitor:HDMI-A-1"
        "7, monitor:DP-2"
        "8, monitor:DP-2"
        "9, monitor:DP-2"
        "10, monitor:DP-2"
      ];
    };
  };
  home.packages = with pkgs; [
    urbit
    # Productivity
    exercism
    vimgolf
    img2pdf
    gparted
    code-cursor
  ];

  features.impermanence.directories = [

    ".cursor"
    ".config/Cursor"
  ];
}
