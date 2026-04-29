{ ... }:
{
  flake.modules.nixos.bluetooth =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [ bluetuith ];
      services.blueman.enable = true;
      hardware.bluetooth.enable = true;
      hardware.bluetooth.settings = {
        General = {
          Enable = "Source,Sink,Media,Socket";
          Experimental = true;
          FastConnectable = true;
        };
        Policy.AutoEnable = true;
      };
      persistence.directories = [ "/var/lib/bluetooth" ];
    };
}
