{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.kanata;
in {
  options.features.kanata.enable = mkEnableOption "kanata config";
  config = mkIf cfg.enable {
    # Enable the uinput module
    boot.kernelModules = [ "uinput" ];

    # Enable uinput
    hardware.uinput.enable = true;

    # Set up udev rules for uinput
    services.udev.extraRules = ''
      KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
    '';

    # Ensure the uinput group exists
    users.groups.uinput = { };

    # Add the Kanata service user to necessary groups
    systemd.services.kanata-internalKeyboard.serviceConfig = {
      SupplementaryGroups = [ "input" "uinput" ];
    };
    services.kanata = {
      enable = true;
      package = pkgs.unstable.kanata;
      keyboards = {
        internalKeyboard = {
          devices = [
            # Replace the paths below with the appropriate device paths for your setup.
            # Use `ls /dev/input/by-path/` to find your keyboard devices.
            # "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
            # "/dev/input/by-path/pci-0000:00:14.0-usb-0:3:1.0-event-kbd"
            "/dev/input/by-path/pci-0000:02:00.0-usb-0:6.3:1.0-event-kbd"
            "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
          ];
          extraDefCfg = "process-unmapped-keys yes";
          config = ''
            (defsrc
              a s d f
              j k l ;
              w o
              caps
            )
            (defvar
              tap-time 200
              hold-time 200
            )

            (defalias
              a-mod (tap-hold $tap-time $hold-time a lmet)
              s-mod (tap-hold $tap-time $hold-time s lalt)
              w-mod (tap-hold $tap-time $hold-time w ralt)
              d-mod (tap-hold $tap-time $hold-time d lsft)
              f-mod (tap-hold $tap-time $hold-time f lctl)
              j-mod (tap-hold $tap-time $hold-time j rctl)
              k-mod (tap-hold $tap-time $hold-time k rsft)
              l-mod (tap-hold $tap-time $hold-time l ralt)
              o-mod (tap-hold $tap-time $hold-time o lalt)
              ;-mod (tap-hold $tap-time $hold-time ; rmet)
              escarrow (tap-hold 200 200 esc (layer-while-held nav))
            )

            (deflayer base
              @a-mod @s-mod @d-mod @f-mod
              @j-mod @k-mod @l-mod @;-mod
              @w-mod @o-mod
              @escarrow
            )
            (deflayermap (nav)
              k up
              h left
              j down
              l right
            )
          '';
        };
      };
    };
  };
}
