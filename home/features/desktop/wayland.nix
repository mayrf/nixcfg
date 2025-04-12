{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.wayland;
in {
  options.features.desktop.wayland.enable =
    mkEnableOption "wayland extra tools and config";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      grim
      hyprlock
      qt6.qtwayland
      slurp
      waypipe
      wf-recorder
      wl-mirror
      wl-clipboard
      wlogout
      wtype
      ydotool

      # gtk3 # For gtk-launch
      # imv
      # mimeo
      # pulseaudio
    ];
    xdg.mimeApps.enable = true;
    home.sessionVariables = {
      MOZ_ENABLE_WAYLAND = 1;
      QT_QPA_PLATFORM = "wayland";
      LIBSEAT_BACKEND = "logind";
    };
    # programs.waybar = {
    #   enable = true;
    #   style = ''
    #     @define-color background-darker rgba(30, 31, 41, 230);
    #     @define-color background #282a36;
    #     @define-color selection #44475a;
    #     @define-color foreground #f8f8f2;
    #     @define-color comment #6272a4;
    #     @define-color cyan #8be9fd;
    #     @define-color green #50fa7b;
    #     @define-color orange #ffb86c;
    #     @define-color pink #ff79c6;
    #     @define-color purple #bd93f9;
    #     @define-color red #ff5555;
    #     @define-color yellow #f1fa8c;

    #     * {
    #         border: none;
    #         border-radius: 0;
    #         font-family: FiraCode Nerd Font;
    #         font-weight: bold;
    #         font-size: 14px;
    #         min-height: 0;
    #     }

    #     window#waybar {
    #         background: rgba(21, 18, 27, 0);
    #         color: #cdd6f4;
    #     }

    #     tooltip {
    #         background: #1e1e2e;
    #         border-radius: 10px;
    #         border-width: 2px;
    #         border-style: solid;
    #         border-color: #11111b;
    #     }

    #     #workspaces button {
    #         padding: 5px;
    #         color: #313244;
    #         margin-right: 5px;
    #     }

    #     #workspaces button.active {
    #         color: #11111b;
    #         background: #a6e3a1;
    #         border-radius: 10px;
    #     }

    #     #workspaces button.focused {
    #         color: #a6adc8;
    #         background: #eba0ac;
    #         border-radius: 10px;
    #     }

    #     #workspaces button.urgent {
    #         color: #11111b;
    #         background: #a6e3a1;
    #         border-radius: 10px;
    #     }

    #     #workspaces button:hover {
    #         background: #11111b;
    #         color: #cdd6f4;
    #         border-radius: 10px;
    #     }

    #     #custom-language,
    #     #custom-updates,
    #     #custom-caffeine,
    #     #custom-weather,
    #     #window,
    #     #clock,
    #     #battery,
    #     #pulseaudio,
    #     #network,
    #     #workspaces,
    #     #tray,
    #     #backlight {
    #         background: #1e1e2e;
    #         padding: 0px 10px;
    #         margin: 3px 0px;
    #         margin-top: 10px;
    #         border: 1px solid #181825;
    #     }

    #     #tray {
    #         border-radius: 10px;
    #         margin-right: 10px;
    #     }

    #     #workspaces {
    #         background: #1e1e2e;
    #         border-radius: 10px;
    #         margin-left: 10px;
    #         padding-right: 0px;
    #         padding-left: 5px;
    #     }

    #     #custom-caffeine {
    #         color: #89dceb;
    #         border-radius: 10px 0px 0px 10px;
    #         border-right: 0px;
    #         margin-left: 10px;
    #     }

    #     #custom-language {
    #         color: #f38ba8;
    #         border-left: 0px;
    #         border-right: 0px;
    #     }

    #     #custom-updates {
    #         color: #f5c2e7;
    #         border-left: 0px;
    #         border-right: 0px;
    #     }

    #     #window {
    #         border-radius: 10px;
    #         margin-left: 60px;
    #         margin-right: 60px;
    #     }

    #     #clock {
    #         color: #fab387;
    #         border-radius: 10px 0px 0px 10px;
    #         margin-left: 0px;
    #         border-right: 0px;
    #     }

    #     #network {
    #         color: #f9e2af;
    #         border-left: 0px;
    #         border-right: 0px;
    #     }

    #     #pulseaudio {
    #         color: #89b4fa;
    #         border-left: 0px;
    #         border-right: 0px;
    #     }

    #     #pulseaudio.microphone {
    #         color: #cba6f7;
    #         border-left: 0px;
    #         border-right: 0px;
    #     }

    #     #battery {
    #         color: #a6e3a1;
    #         border-radius: 0 10px 10px 0;
    #         margin-right: 10px;
    #         border-left: 0px;
    #     }

    #     #custom-weather {
    #         border-radius: 0px 10px 10px 0px;
    #         border-right: 0px;
    #         margin-left: 0px;
    #     }
    #   '';
    #   settings = {
    #     mainbar = {
    #       layer = "top";
    #       position = "top";
    #       mod = "dock";
    #       exclusive = true;
    #       passthrough = false;
    #       gtk-layer-shell = true;
    #       height = 0;
    #       modules-left = [ "clock" "custom/weather" "hyprland/workspaces" ];
    #       modules-center = [ "hyprland/window" ];
    #       modules-right = [ "tray" ];

    #       "hyprland/window" = {
    #         format = "👉 {}";
    #         seperate-outputs = true;
    #       };
    #       "hyprland/workspaces" = {
    #         disable-scroll = true;
    #         all-outputs = true;
    #         on-click = "activate";
    #         format = " {name} {icon} ";
    #         on-scroll-up = "hyprctl dispatch workspace e+1";
    #         on-scroll-down = "hyprctl dispatch workspace e-1";
    #         format-icons = {
    #           "1" = "";
    #           "2" = "";
    #           "3" = "";
    #           "4" = "";
    #           "5" = "";
    #           "6" = "";
    #           "7" = "";
    #         };
    #         persistent_workspaces = {
    #           "1" = [ ];
    #           "2" = [ ];
    #           "3" = [ ];
    #           "4" = [ ];
    #         };
    #       };
    #       "custom/weather" = {
    #         format = "{}°C";
    #         tooltip = true;
    #         interval = 3600;
    #         exec = "wttrbar --location Pockau-Lengefeld";
    #         return-type = "json";
    #       };
    #       tray = {
    #         icon-size = 13;
    #         spacing = 10;
    #       };
    #       clock = {
    #         format = " {:%R   %d/%m}";
    #         tooltip-format = ''
    #           <big>{:%Y %B}</big>
    #           <tt><small>{calendar}</small></tt>'';
    #       };
    #     };
    #   };
    # };
    # wayland.windowManager.hyprland = {
    #   enable = true;
    #   settings = {
    #     xwayland = { force_zero_scaling = true; };

    #     exec-once = [
    #       "waybar"
    #       "hyprpaper"
    #       "hypridle"
    #       ''
    #         wl-paste -p -t text --watch clipman store -P --histpath="~/.local/share/clipman-primary.json"''
    #     ];

    #     env =
    #       [ "XCURSOR_SIZE,32" "WLR_NO_HARDWARE_CURSORS,1" "GTK_THEME,Dracula" ];

    #     input = {
    #       kb_layout = "us";
    #       kb_variant = "";
    #       kb_model = "";
    #       kb_rules = "";
    #       kb_options = "ctrl:nocaps";
    #       follow_mouse = 1;

    #       touchpad = { natural_scroll = true; };

    #       sensitivity = 0;
    #     };

    #     general = {
    #       gaps_in = 5;
    #       gaps_out = 5;
    #       border_size = 1;
    #       "col.active_border" = "rgba(9742b5ee) rgba(9742b5ee) 45deg";
    #       "col.inactive_border" = "rgba(595959aa)";
    #       layout = "dwindle";
    #     };

    #     decoration = {
    #       "col.shadow" = "rgba(1E202966)";
    #       drop_shadow = true;
    #       shadow_range = 60;
    #       shadow_offset = "1 2";
    #       shadow_render_power = 3;
    #       shadow_scale = 0.97;
    #       rounding = 8;
    #       blur = {
    #         enabled = true;
    #         size = 3;
    #         passes = 3;
    #       };
    #       active_opacity = 0.9;
    #       inactive_opacity = 0.5;
    #     };

    #     animations = {
    #       enabled = true;
    #       bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
    #       animation = [
    #         "windows, 1, 7, myBezier"
    #         "windowsOut, 1, 7, default, popin 80%"
    #         "border, 1, 10, default"
    #         "borderangle, 1, 8, default"
    #         "fade, 1, 7, default"
    #         "workspaces, 1, 6, default"
    #       ];
    #     };

    #     dwindle = {
    #       pseudotile = true;
    #       preserve_split = true;
    #     };

    #     master = { };

    #     gestures = { workspace_swipe = false; };

    #     windowrule = [
    #       "float, file_progress"
    #       "float, confirm"
    #       "float, dialog"
    #       "float, download"
    #       "float, notification"
    #       "float, error"
    #       "float, splash"
    #       "float, confirmreset"
    #       "float, title:Open File"
    #       "float, title:branchdialog"
    #       "float, Lxappearance"
    #       "float, Wofi"
    #       "float, dunst"
    #       "animation none,Wofi"
    #       "float,viewnior"
    #       "float,feh"
    #       "float, pavucontrol-qt"
    #       "float, pavucontrol"
    #       "float, file-roller"
    #       "fullscreen, wlogout"
    #       "float, title:wlogout"
    #       "fullscreen, title:wlogout"
    #       "idleinhibit focus, mpv"
    #       "idleinhibit fullscreen, firefox"
    #       "float, title:^(Media viewer)$"
    #       "float, title:^(Volume Control)$"
    #       "float, title:^(Picture-in-Picture)$"
    #       "size 800 600, title:^(Volume Control)$"
    #       "move 75 44%, title:^(Volume Control)$"
    #     ];

    #     "$mainMod" = "SUPER";

    #     bind = [
    #       "$mainMod, return, exec, kitty -e zellij-ps"
    #       "$mainMod, t, exec, kitty -e fish -c 'neofetch; exec fish'"
    #       "$mainMod SHIFT, e, exec, kitty -e zellij_nvim"
    #       "$mainMod, o, exec, thunar"
    #       "$mainMod, Escape, exec, wlogout -p layer-shell"
    #       "$mainMod, Space, togglefloating"
    #       "$mainMod, q, killactive"
    #       "$mainMod, M, exit"
    #       "$mainMod, F, fullscreen"
    #       "$mainMod, V, togglefloating"
    #       "$mainMod, D, exec, wofi --show drun --allow-images"
    #       "$mainMod SHIFT, S, exec, bemoji"
    #       "$mainMod, P, exec, wofi-pass"
    #       "$mainMod SHIFT, P, pseudo"
    #       "$mainMod, J, togglesplit"
    #       "$mainMod, left, movefocus, l"
    #       "$mainMod, right, movefocus, r"
    #       "$mainMod, up, movefocus, u"
    #       "$mainMod, down, movefocus, d"
    #       "$mainMod, 1, workspace, 1"
    #       "$mainMod, 2, workspace, 2"
    #       "$mainMod, 3, workspace, 3"
    #       "$mainMod, 4, workspace, 4"
    #       "$mainMod, 5, workspace, 5"
    #       "$mainMod, 6, workspace, 6"
    #       "$mainMod, 7, workspace, 7"
    #       "$mainMod, 8, workspace, 8"
    #       "$mainMod, 9, workspace, 9"
    #       "$mainMod, 0, workspace, 10"
    #       "$mainMod SHIFT, 1, movetoworkspace, 1"
    #       "$mainMod SHIFT, 2, movetoworkspace, 2"
    #       "$mainMod SHIFT, 3, movetoworkspace, 3"
    #       "$mainMod SHIFT, 4, movetoworkspace, 4"
    #       "$mainMod SHIFT, 5, movetoworkspace, 5"
    #       "$mainMod SHIFT, 6, movetoworkspace, 6"
    #       "$mainMod SHIFT, 7, movetoworkspace, 7"
    #       "$mainMod SHIFT, 8, movetoworkspace, 8"
    #       "$mainMod SHIFT, 9, movetoworkspace, 9"
    #       "$mainMod SHIFT, 0, movetoworkspace, 10"
    #       "$mainMod, mouse_down, workspace, e+1"
    #       "$mainMod, mouse_up, workspace, e-1"
    #     ];

    #     bindm = [
    #       "$mainMod, mouse:272, movewindow"
    #       "$mainMod, mouse:273, resizewindow"
    #     ];

    #     windowrulev2 = [
    #       "workspace 1,class:(Emacs)"
    #       "workspace 3,opacity 1.0, class:(brave-browser)"
    #       "workspace 4,class:(com.obsproject.Studio)"
    #     ];
    #   };
    # };
  };
}
