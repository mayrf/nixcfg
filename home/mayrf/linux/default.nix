{ lib, config, ... }: {
  imports = [ ../global/programs.nix ];

  services = {
    nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
    gpg-agent = {
      enable = true;
      pinentryFlavor = "qt";
    };
  };

  home = {
    username = lib.mkDefault "mayrf";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "23.11";
    sessionPath = [ "$HOME/.local/bin" ];
  };
  # For virtualisation
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };

  # persistence = {
  #   "/persist/home/mayrf" = {
  #     directories = [
  #       "Documents"
  #       "Downloads"
  #       "Pictures"
  #       "Videos"
  #       ".local/bin"
  #     ];
  #     allowOther = true;
  #   };
  # };
}
