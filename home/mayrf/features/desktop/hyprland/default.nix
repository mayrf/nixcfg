{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ../common
    ../common/wayland-wm
    ./tty-init.nix
    ./basic-binds.nix
    ./systemd-fixes.nix
  ];

  xdg.configFile."xkb/rules/evdev".text = ''
    ! option = symbols
      hungarian_letters:huletters    = +hungarian_letters(huletters)
    ! include %S/evdev
  '';
  xdg.configFile."xkb/symbols/hungarian_letters".text = ''
    xkb_symbols "huletters" {
        //ä on alt+a
        key <AC01> { [     a,   A, adiaeresis,  Adiaeresis      ]   };
        //á on alt+q
        key <AD01> { [     q,   Q, aacute,      Aacute          ]   };

        //ü on alt+u
        key <AD07> { [     u,   U, udiaeresis,  Udiaeresis      ]   };
        //ü on alt+j
        key <AC07> { [     j,   J, udoubleacute,  Udoubleacute      ]   };
        //ú on alt+y
        key <AD06> { [     y,   Y, uacute,      Uacute          ]   };

        //ö on alt+o
        key <AD09> { [     o,   O, odiaeresis,  Odiaeresis      ]   };
        //ő on alt+p
        key <AD10> { [     p,   P, odoubleacute,  Odoubleacute      ]   };
        //ó on alt+l
        key <AC09> { [     l,   L, oacute,      Oacute      ]   };


        // make right alt altGr
        include "level3(ralt_switch)"
    };  '';
  # TODO set up cliphist
  home.packages = with pkgs; [ cliphist hyprpicker brightnessctl xorg.xhost ];
  wayland.windowManager.hyprland = {
    enable = true;
    package =
      inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;

    settings = {
      general = {
        layout = "master";
        gaps_in = 1;
        gaps_out = 1;
        border_size = 1;
      };
      misc = { focus_on_activate = true; };
      input = {
        kb_layout = "us";
        kb_variant = "altgr-intl";
        kb_options = "hungarian_letters:huletters";
        repeat_rate = 50;
        repeat_delay = 250;
      };

      decoration = {
        active_opacity = 1.0;
        # active_opacity = 0.94;
        inactive_opacity = 0.84;
        fullscreen_opacity = 1.0;
        blur = {
          size = 5;
          passes = 3;
          new_optimizations = true;
          ignore_opacity = true;
        };
      };
      animations = { enabled = false; };

      exec = [ "${pkgs.swaybg}/bin/swaybg -i ${config.wallpaper} --mode fill" ];

      exec-once = [
        "wl-paste --type text --watch cliphist store" # Stores only text data
        "wl-paste --type image --watch cliphist store" # Stores only image data
        "${pkgs.libsForQt5.polkit-kde-agent}/libexec/polkit-kde-authentication-agent-1"

      ];

      bind = let
        swaylock = "${config.programs.swaylock.package}/bin/swaylock";
        playerctl = "${config.services.playerctld.package}/bin/playerctl";
        playerctld = "${config.services.playerctld.package}/bin/playerctld";
        # makoctl = "${config.services.mako.package}/bin/makoctl";
        wofi = "${config.programs.wofi.package}/bin/wofi";
        # pass-wofi = "${pkgs.pass-wofi.override {
        # pass = config.programs.password-store.package;
        # }}/bin/pass-wofi";

        grimblast = "${pkgs.grimblast}/bin/grimblast";
        pactl = "${pkgs.pulseaudio}/bin/pactl";

        gtk-launch = "${pkgs.gtk3}/bin/gtk-launch";
        xdg-mime = "${pkgs.xdg-utils}/bin/xdg-mime";
        defaultApp = type: "${gtk-launch} $(${xdg-mime} query default ${type})";

        # terminal = config.home.sessionVariables.TERMINAL;
        # browser = defaultApp "x-scheme-handler/https";
        # editor = defaultApp "text/plain";
        terminal = "${pkgs.kitty}/bin/kitty";
        browser = "${pkgs.librewolf}/bin/librewolf";
        brave = "${pkgs.brave}/bin/brave";
        editor = "${pkgs.emacs}/bin/emacsclient -c";
        hyprctl = "${pkgs.hyprland}/bin/hyprctl";

      in [
        # Program bindings
        "SUPER,Return,exec,${terminal}"
        "SUPER,e,exec,${editor}"
        "SUPER,w,exec,${browser}"
        "SUPERSHIFT,w,exec,${brave}"
        "SUPER,r,exec,${terminal} lf"
        "SUPERSHIFT,C,exec,${terminal} sudo nmtui"
        "SUPERSHIFT, R, exec,${hyprctl} reload"
        # Brightness control (only works if the system has lightd)
        ",XF86MonBrightnessUp,exec,brightnessctl set 5%+"
        ",XF86MonBrightnessDown,exec,brightnessctl set 5%-"
        # Volume
        ",XF86AudioRaiseVolume,exec,${pactl} set-sink-volume @DEFAULT_SINK@ +5%"
        ",XF86AudioLowerVolume,exec,${pactl} set-sink-volume @DEFAULT_SINK@ -5%"
        ",XF86AudioMute,exec,${pactl} set-sink-mute @DEFAULT_SINK@ toggle"
        "SHIFT,XF86AudioMute,exec,${pactl} set-source-mute @DEFAULT_SOURCE@ toggle"
        ",XF86AudioMicMute,exec,${pactl} set-source-mute @DEFAULT_SOURCE@ toggle"
        # Screenshotting
        ",Print,exec,${grimblast} --notify copy output"
        "SHIFT,Print,exec,${grimblast} --notify copy active"
        "CONTROL,Print,exec,${grimblast} --notify copy screen"
        "SUPER,Print,exec,${grimblast} --notify copy window"
        "ALT,Print,exec,${grimblast} --freeze --notify copy area"
        "SUPERSHIFT,p,exec,${grimblast} --freeze --notify copy area"
      ] ++

      (lib.optionals config.services.playerctld.enable [
        # Media control
        ",XF86AudioNext,exec,${playerctl} next"
        ",XF86AudioPrev,exec,${playerctl} previous"
        ",XF86AudioPlay,exec,${playerctl} play-pause"
        ",XF86AudioStop,exec,${playerctl} stop"
        "ALT,XF86AudioNext,exec,${playerctld} shift"
        "ALT,XF86AudioPrev,exec,${playerctld} unshift"
        "ALT,XF86AudioPlay,exec,systemctl --user restart playerctld"
      ]) ++
      # Screen lock
      (lib.optionals config.programs.swaylock.enable [
        ",XF86Launch5,exec,${swaylock} -S"
        ",XF86Launch4,exec,${swaylock} -S"
        "SUPER,backspace,exec,${swaylock} -S"
      ]) ++
      # Notification manager
      # (lib.optionals config.services.mako.enable [
      #   "SUPER,w,exec,${makoctl} dismiss"
      # ]) ++

      # Launcher
      (lib.optionals config.programs.wofi.enable [
        "SUPER,x,exec,${wofi} -S drun -x 10 -y 10 -W 25% -H 60%"
        "SUPER,d,exec,${wofi} -S drun"
        "SUPERSHIFT,d,exec,${wofi} -S drun"
        "SUPER, V, exec, cliphist list | wofi --dmenu | cliphist decode | wl-copy"
      ]
      # ++ (lib.optionals config.programs.password-store.enable [
      #   ",Scroll_Lock,exec,${pass-wofi}" # fn+k
      #   ",XF86Calculator,exec,${pass-wofi}" # fn+f12
      #   "SUPER,semicolon,exec,pass-wofi"
      # ])
      );

      #monitor = ",preferred,auto,1";
      monitor = map (m:
        let
          resolution = "${toString m.width}x${toString m.height}@${
              toString m.refreshRate
            }";
          position = "${toString m.x}x${toString m.y}";
        in "${m.name},${
          if m.enabled then "${resolution},${position},1" else "disable"
        }") (config.monitors);

      workspace = map (m: "${m.name},${m.workspace}")
        (lib.filter (m: m.enabled && m.workspace != null) config.monitors);

    };
    # This is order sensitive, so it has to come here.
    extraConfig = ''
      # Passthrough mode (e.g. for VNC)
      bind=SUPER,P,submap,passthrough
      submap=passthrough
      bind=SUPER,P,submap,reset
      submap=reset
    '';
  };
}
