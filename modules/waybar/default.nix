{ ... }:
{
  flake.modules.homeManager.waybar =
    { config, lib, pkgs, ... }:
    let
      cat = "${pkgs.coreutils}/bin/cat";
      cut = "${pkgs.coreutils}/bin/cut";
      grep = "${pkgs.gnugrep}/bin/grep";
      jq = "${pkgs.jq}/bin/jq";
      pavucontrol = "${pkgs.pavucontrol}/bin/pavucontrol";
      wofi = "${pkgs.wofi}/bin/wofi";

      jsonOutput = name:
        { pre ? "", text ? "", tooltip ? "", alt ? "", class ? "", percentage ? ""
        }:
        "${
          pkgs.writeShellScriptBin "waybar-${name}" ''
            set -euo pipefail
            ${pre}
            ${jq} -cn \
              --arg text "${text}" \
              --arg tooltip "${tooltip}" \
              --arg alt "${alt}" \
              --arg class "${class}" \
              --arg percentage "${percentage}" \
              '{text:$text,tooltip:$tooltip,alt:$alt,class:$class,percentage:$percentage}'
          ''
        }/bin/waybar-${name}";
    in
    {
      home.packages = [
        pkgs.pulseaudio
      ];
      programs.waybar = {
        enable = true;
        systemd.enable = true;
        settings = {
          mainbar = {
            layer = "bottom";
            position = "bottom";
            modules-left = [ "hyprland/workspaces" "hyprland/submap" ];
            modules-center = [
              "cpu"
              "custom/separator"
              "memory"
              "custom/separator"
              "clock"
            ];
            modules-right = [
              "hyprland/window"
              "custom/separator"
              "network"
              "custom/separator"
              "battery"
              "custom/separator"
              "tray"
              "custom/separator"
              "custom/hostname"
            ];
            battery = {
              format = "{capacity}% {icon}";
              "format-icons" = [ "" "" "" "" "" ];
              interval = 60;
              format-charging = "󰂄 {capacity}%";
            };
            clock = { "format-alt" = "{:%a, %d. %b  %H:%M}"; };
            "hyprland/window" = {
              rewrite = {
                "(.*) - Brave" = "Brave  - $1";
                "Brave" = "Brave ";
                "(.*) – Doom Emacs" = "  $1";
                "(.*) — LibreWolf" = "LibreWolf  - $1";
                "LibreWolf" = "LibreWolf ";
                "(.*) — Mozilla Firefox" = "󰈹  $1";
                "Mozilla Firefox" = "󰈹";
                "(.*) - Mozilla Thunderbird" = "  $1";
                "vim (.*)" = "  $1";
              };
            };
            cpu = { format = "󰍛   {usage}%"; };
            "custom/gpu" = {
              interval = 5;
              return-type = "json";
              exec = jsonOutput "gpu" {
                text = "$(${cat} /sys/class/drm/card1/device/gpu_busy_percent)";
                tooltip = "GPU Usage";
              };
              format = "󰒋  {}%";
            };
            memory = {
              format = "  {}%";
              interval = 5;
            };
            pulseaudio = {
              format = "{icon}  {volume}%";
              format-muted = "   0%";
              format-icons = {
                headphone = "󰋋";
                headset = "󰋎";
                portable = "";
                default = [ "" "" "" ];
              };
              on-click = pavucontrol;
            };
            network = {
              interval = 3;
              format-wifi = "   {essid}";
              format-ethernet = "󰈁 Connected";
              format-disconnected = "";
              tooltip-format = ''
                {ifname}
                {ipaddr}/{cidr}
                Up: {bandwidthUpBits}
                Down: {bandwidthDownBits}'';
              on-click = "";
            };
            "custom/separator" = { exec = "echo '|'"; };
            "custom/menu" = {
              return-type = "json";
              exec = jsonOutput "menu" {
                text = "";
                tooltip = ''
                  $(${cat} /etc/os-release | ${grep} PRETTY_NAME | ${cut} -d '"' -f2)'';
              };
              on-click = "${wofi} -S drun -x 10 -y 10 -W 25% -H 60%";
            };
            "custom/hostname" = { exec = "echo $USER@$HOSTNAME"; };
          };
        };
        style = let
          inherit (config.colorscheme) palette;
        in ''
          * {
              font-family: Roboto, ${config.fontProfiles.regular.family}, ${config.fontProfiles.monospace.family};
              padding: 0 7px;
              border: none;
              border-radius: 0;
              font-family: FiraCode Nerd Font;
              font-weight: bold;
              font-size: 13px;
              min-height: 0;
          }
          #custom-separator {
            padding: 0 0;
            color: #000000;
          }

          #tray {
            color: #000000;
          }
        '';
      };
    };
}
