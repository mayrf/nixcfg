{ inputs, ... }:

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

  features = {
    ensure-secrets-repo.enable = true;
    ensure-private-config-repo.enable = true;
    ensure-config-repo.enable = true;
    impermanence.enable = true;
    cli = {
      zsh.enable = true;
      fzf.enable = true;
      # ai.enable = true;
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
    desktop = {
      wayland.enable = true;
      waybar.enable = true;
      hyprland.enable = true;
      gammastep.enable = true;
      mako.enable = true;
      wofi.enable = true;
      nextcloud-client.enable = true;
      opencloud-client.enable = true;
      virtualisation.enable = true;
      postman.enable = true;
      librewolf.enable = true;
      gpg.enable = true;
      zathura.enable = true;
      learning.enable = true;
      media.enable = true;
      social.enable = true;
      productivity.enable = true;
    };
    terminal = {
      ghostty.enable = true;
    };
    editor = {
      nvim.enable = true;
      emacs.enable = true;
      # vscode.enable = true;
    };
    private = {
      work.enable = true;
    };
  };

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
}
