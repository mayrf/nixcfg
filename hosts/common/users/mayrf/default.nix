{ pkgs, config, user, ... }:
let
  ifTheyExist = groups:
    builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
in {
  programs.zsh.enable = true;
  programs.steam.enable = true;
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs;
    [
      # Add any missing dynamic libraries for unpackaged
      # programs here, NOT in environment.systemPackages
    ];

  users.mutableUsers = true;
  users.users.${user} = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "video" "audio" "plugdev" "lp" ]
      ++ ifTheyExist [ "network" "docker" "libvirtd" "deluge" ];

    # passwordFile = config.sops.secrets.mayrf_password.path;
    initialPassword = "password";
    packages = [ pkgs.home-manager ];
  };

  # sops.secrets.mayrf_password = {
  #   sopsFile = ../../secrets/secrets.yaml;
  #   neededForUsers = true;
  # };

  services.geoclue2.enable = true;
  security.pam.services = { swaylock = { }; };
}
