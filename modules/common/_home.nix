{ inputs, lib, pkgs, config, outputs, host, ... }:
let inherit (inputs.nix-colors) colorSchemes;
in {
  imports = [
    inputs.nix-colors.homeManagerModule
    ./_mime-apps.nix
  ];

  services = {
    gpg-agent = {
      enable = true;
      pinentry.package = pkgs.pinentry-qt;
    };
  };

  programs.ssh.includes = [ "~/.ssh/config.local" ];

  home = {
    username = host.username;
    homeDirectory = lib.mkDefault "/home/${host.username}";
    sessionPath = [ "$HOME/.local/bin" ];
  };

  features.impermanence.directories = [
    ".ssh"
    ".gnupg"
    ".local/share/keyrings"
    "Downloads"
    "Documents"
    "playground"
    "code"
    "cloud"
    ".local/share/fonts"
  ];

  home.file = {
    ".local/bin" = {
      source = ./scripts;
      recursive = true;
    };
  };

  nix = {
    settings = {
      warn-dirty = false;
      keep-outputs = true;
      experimental-features = [ "nix-command" "flakes" ];
    };

    registry = {
      "mytemplates" = {
        from = {
          id = "mytemplates";
          type = "indirect";
        };
        to = {
          path = "${config.xdg.configHome}/nixcfg";
          type = "path";
        };
      };
    };
  };
  systemd.user.startServices = "sd-switch";

  programs = { home-manager.enable = true; };

  home.file.".colorscheme".text = config.colorscheme.slug;

  fonts.fontconfig.enable = true;

  xdg = {
    enable = true;
    configHome = "${config.home.homeDirectory}/.config";
    cacheHome = "${config.home.homeDirectory}/.cache";
    dataHome = "${config.home.homeDirectory}/.local/share";
    stateHome = "${config.home.homeDirectory}/.local/state";
  };
}
