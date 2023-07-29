{ pkgs, config, user, ... }:
let ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
in
{
  programs.zsh.enable = true;
  users.mutableUsers = true;
  users.users.${user} = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [
      "wheel"
      "video"
      "audio"
    ] ++ ifTheyExist [
      "network"
      "docker"
      "libvirtd"
      "deluge"
    ];

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
