{ lib, config, pkgs, ... }: {
  imports = [ ../global/programs.nix ];

  services = {
    nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
    gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-qt;
    };
  };

  stylix.targets.emacs.enable = false;

  home = {
    username = lib.mkDefault "mayrf";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    # stateVersion = lib.mkDefault "24.11";
    stateVersion = lib.mkDefault "25.05";
    sessionPath = [ "$HOME/.local/bin" ];
  };
  # For virtualisation
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };
}
