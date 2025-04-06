{  ... }:
{
  imports = [
    ./configuration.nix
  ];
  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "yttrium";
    flakeDir = "/etc/nixos";
    persistDir = "/persist/system";
    persistDirRoot = "/persist";
    isImpermanent = true;
    sysStateVersion = "25.05";
  };
}
