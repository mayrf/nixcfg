{ config, pkgs, ... }:
#imports = [
#  ./colors.nix
#  ./modules.nix
#];
#
let
  mypolybar =
    pkgs.polybar.override {
      # Extra packages to run polybar (mostly sound atm)
      alsaSupport = true;
      pulseSupport = true;
    };
in
{
  systemd.user.services.polybar.Install.WantedBy =
    [ "graphical-session.target" ];
  services.polybar = {
    enable = true;
    script = ''
      polybar main &
    '';
    package = mypolybar;
    config = {
      "colors" = {
        background = "#00";
        # background-alt = "#3306080a";
        background-alt = "#000000";
        primary = "#fff";
        alert = "#f7768e";
      };
      "bar/main" = {
        bottom = "false";

        background = "\${colors.background}";
        border-size = 0;
        border-color = "#00000000";

        module-margin-left = 1;
        module-margin-right = 1;
        radius = "0.0";
        padding-right = "2";

        font-0 = "JetBrainsMono Nerd Font:style=bold:size=10;2";

        modules-left = "bspwm xwindow";
        modules-right = "date filesystem wlan battery pulseaudio";

        wm-restack = "bspwm";

      };
      "module/xwindow" = {
        type = "internal/xwindow";
        label = "%title:0:30:...%";
      };
      "module/filesystem" = {
        type = "internal/fs";
        interval = 25;
        mount-0 = "/";
        format-mounted = "<label-mounted>";
        format-mounted-prefix = "";
        format-mounted-prefix-foreground = "\${colors.background-alt}";
        format-mounted-prefix-padding = 1;
        label-mounted = "%free%";

        format-unmounted = "<label-unmounted>";
        format-unmounted-prefix = "";
        label-unmounted = "%mountpoint%: not mounted";
      };

      "module/pulseaudio" = {
        type = "internal/pulseaudio";
        use-ui-max = true;
        interval = 5;
        click-right = "pavucontrol";
        format-volume = "<ramp-volume> <label-volume>";
        label-muted = "󰝟";
        label-muted-foreground = "#666";
        ramp-volume-0 = "󰕿";
        ramp-volume-1 = "󰖀";
        ramp-volume-2 = "󰕾";
      };
      "module/battery" = {
        type = "internal/battery";
        full-at = 99;
        low-at = 5;
        poll-interval = 5;
        battery = "BAT0";
        adapter = "ADP1";
        format-charging = "󰂄 <label-charging>";
        format-discharging = "<ramp-capacity> <label-discharging>";
        #label-charging = "Charging %percentage%";
        #label-discharging = "Discharging %percentage%";
        ramp-capacity-0 = " ";
        ramp-capacity-1 = " ";
        ramp-capacity-2 = " ";
        ramp-capacity-3 = " ";
        ramp-capacity-4 = " ";
      };
      "module/date" = {
        type = "internal/date";
        interval = 5;
        time = "%H:%M";
        date = "%a %b %d";
        format = "󰅐 <label>";
        label = "%time% %date%";
      };
      "module/bspwm" = {
        type = "internal/bspwm";
        pin-workspaces = "true";

        # label-focused = "%index%";
        label-focused = "%index%";
        label-focused-backgroud = "#000000";
        label-focused-padding = 2;

        label-occupied = "%index%";
        label-occupied-padding = 2;

        label-urgent = "%index%!";
        label-urgent-backgroud = "\${colors.alert}";
        label-urgent-padding = 2;

        label-empty = "";
      };

      "module/wlan" = {
        type = "internal/network";
        interface-type = "wireless";
        interval = "3.0";
        format-connected = "%{A1:networkmanager_dmenu:}󰖩<label-connected>%{A}";
        label-connected = "%{A1:wifimenu:}%essid%%{A}";
        label-connected-padding = 1;

        format-disconnected = "<label-disconnected>";
        format-disconnected-padding = 1;
        label-disconnected = "%{A1:wifimenu:}%{A}";
        label-disconnected-foreground = "\${colors.primary}";
        label-disconnected-padding = 1;
      };
      #"settings" = {
      # screenchange-reload = "true";
      #};
    };
  };
  home.file.".config/polybar/script/mic.sh" = {
    # Custom script: Mic mute
    text = ''
      #!/bin/sh

      case $1 in
          "status")
          #MUTED=$(pacmd list-sources | awk '/\*/,EOF {print}' | awk '/muted/ {print $2; exit}')
          #if [[ $MUTED = "no" ]]; then
          MUTED=$(awk -F"[][]" '/Left:/ { print $4 }' <(amixer sget Capture))
          if [[ $MUTED = "on" ]]; then
              echo ''
          else
              echo ''
          fi
          ;;
          "toggle")
          #ID=$(pacmd list-sources | grep "*\ index:" | cut -d' ' -f5)
          #pactl set-source-mute $ID toggle
          ${pkgs.alsa-utils}/bin/amixer set Capture toggle
          ;;
      esac
    '';
    executable = true;
  };

}
