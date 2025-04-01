{ pkgs, config,  ... }:
let
  ifTheyExist = groups:
    builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
in {
  programs.zsh.enable = true;
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs;
    [
      # Add any missing dynamic libraries for unpackaged
      # programs here, NOT in environment.systemPackages
    ];

  users.mutableUsers = false;
  users.users.${config.hostSpec.username} = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "video" "audio" "plugdev" "lp" ]
      ++ ifTheyExist [ "network" "docker" "libvirtd" "deluge" ];

    hashedPasswordFile = config.sops.secrets."mayrf/hashedPassword".path;
    # initialPassword = "password";
    packages = [ pkgs.home-manager ];
  };

  # sops.secrets.mayrf_password = {
  #   sopsFile = ../../secrets/secrets.yaml;
  #   neededForUsers = true;
  # };

  security.pam.services = { swaylock = { }; };
}
