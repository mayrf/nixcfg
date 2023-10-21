{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    lutris
    wine
  ];

  systemd.extraConfig = ''
    DefaultLimitNOFILE=1048576
  '';

  systemd.user.extraConfig = ''
    DefaultLimitNOFILE=1048576
  '';
}
