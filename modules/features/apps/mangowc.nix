{ self, lib, ... }:
{

  flake.modules.nixos.mango = { pkgs, ... }: let
    mangoPackage = self.packages.${pkgs.stdenv.hostPlatform.system}.mangowc;
  in {
    # imports = [ inputs.mangowm.nixosModules.mango ];

    # programs.mango = {
    #   enable = true;
    #   package = mangoPackage;
    # };
    environment.systemPackages = [
      mangoPackage
    ];
    services.greetd = {
      enable = true;
      settings = {
        initial_session = {
          command = "${mangoPackage}/bin/mango";
          user = "mayrf";
        };
        default_session = {
          command = "${pkgs.tuigreet}/bin/tuigreet --cmd ${mangoPackage}/bin/mango";
          user = "greeter";
        };
      };
    };
  };


  flake.wrappers.mangowc = {
    wlib,
    pkgs,
    config,
    # host,  # you'll need to thread this through however your flake exposes per-host values
    ...
  }: let
    mod = "SUPER";
    # terminal = "${pkgs.unstable.ghostty}/bin/ghostty";
    terminal = "${pkgs.alacritty}/bin/alacritty";
    # terminal-exec = "${pkgs.unstable.ghostty}/bin/ghostty -e";

    browser   = lib.getExe pkgs.firefox;
    brave     = "${pkgs.brave}/bin/brave";
    editor    = "vanemacs";

    fuzzel    = "${pkgs.fuzzel}/bin/fuzzel";
    grimblast = "${pkgs.grimblast}/bin/grimblast";
    pactl     = "${pkgs.pulseaudio}/bin/pactl";
    brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";

    workspaces = (map toString (lib.range 0 9)) ++ (map (n: "F${toString n}") (lib.range 1 12));

    directions = {
      h = "left"; l = "right"; k = "up"; j = "down";
                                         Left = "left"; Right = "right"; Up = "up"; Down = "down";
    };
  in {
    imports = [ wlib.wrapperModules.mangowc ];

    config = {
      package = pkgs.mangowc;

      autostart_sh = ''
        wl-paste --type text --watch ${pkgs.cliphist}/bin/cliphist store &
        wl-paste --type image --watch ${pkgs.cliphist}/bin/cliphist store &
        ${pkgs.kdePackages.polkit-kde-agent-1}/libexec/polkit-kde-authentication-agent-1 &
        # hypridle &
         # ${lib.getExe self.packages.${pkgs.stdenv.hostPlatform.system}.myNoctalia}
        noctalia
      '';

      settings = {
        # -------------------------------------------------------------------
        # Appearance
        # -------------------------------------------------------------------
        blur             = 1;
        blur_params_num_passes = 2;
        blur_params_radius     = 3;

        shadows          = 0;
        border_radius    = 20;
        focused_opacity  = 1.0;
        unfocused_opacity = 0.84;

        animations       = 0;

        borderpx         = 1;
        bordercolor      = "0x9d8b70ff";
        focuscolor       = "0x88a4d3ff";
        rootcolor        = "0x231e18ff";

        # -------------------------------------------------------------------
        # Layout
        # -------------------------------------------------------------------
        gappih = 5;
        gappiv = 5;
        gappoh = 10;
        gappov = 10;
        smartgaps          = 1;
        no_border_when_single = 1;
        default_mfact      = 0.55;
        new_is_master      = 1;

        # -------------------------------------------------------------------
        # Input
        # -------------------------------------------------------------------
        xkb_rules_layout  = "us";
        xkb_rules_options = "hungarian_letters:huletters";
        # NOTE: your custom XKB files still need to be installed via
        # xdg.configFile in a home-manager module alongside this wrapper.
        repeat_delay = 250;
        repeat_rate  = 50;

        tap_to_click  = 1;
        tap_and_drag  = 1;
        drag_lock     = 1;
        trackpad_natural_scrolling = 0;

        # -------------------------------------------------------------------
        # Misc
        # -------------------------------------------------------------------
        focus_on_activate = 1;
        sloppyfocus       = 0;
        warpcursor        = 1;

        # -------------------------------------------------------------------
        # Window rules
        # -------------------------------------------------------------------
        # NOTE: verify scratchpad sizing syntax against mango docs —
        # the ratio options below are mango-native alternatives to
        # hyprland's (monitor_w*0.9) expressions.
        scratchpad_width_ratio  = 0.9;
        scratchpad_height_ratio = 0.9;

        # -------------------------------------------------------------------
        # Keybinds
        # -------------------------------------------------------------------
        bind =
          [
            # Session
            "${mod}+SHIFT,q,killclient"
            "${mod},m,quit"

            # Layouts
            "${mod},f,togglemaximizescreen"
            "${mod}+SHIFT,f,togglefullscreen"
            "${mod}+SHIFT,space,togglefloating"

            # Master layout resizing
            "${mod},j,focusdir,down"
            "${mod},k,focusdir,up"

            # Scratchpad terminal
            "${mod},T,spawn,${terminal} --title=scratch_term"
            "${mod},T,toggle_scratchpad"

            # Scratchpad emacs
            # "${mod},B,spawn,dotemacs -c --frame-parameters='(quote (name . \"scratch_emacs\"))'"
            # "${mod},B,toggle_scratchpad"

            # Org-capture
            "${mod}+SHIFT,C,spawn,dotemacs-org-capture"

            # Programs
            "${mod},Return,spawn,${terminal}"
            "${mod},e,spawn,${editor}"
            # "${mod}+SHIFT,e,spawn,emacsclient -s vanilla -c"
            "${mod},w,spawn,${browser}"
            "${mod}+SHIFT,w,spawn,${brave}"
            # "${mod},r,spawn,${terminal-exec} yazi"
            # "${mod}+SHIFT,n,spawn,${terminal-exec} sudo nmtui"
            # "${mod}+SHIFT,r,reload_config"

            # Fuzzel launcher
            "${mod},d,spawn,${fuzzel}"
            "${mod}+SHIFT,d,spawn,${fuzzel}"

            # Clipboard
            # "${mod},v,spawn,cliphist list | ${fuzzel} --dmenu | cliphist decode | wl-copy"

            # Power menu
            # "${mod}+SHIFT,Backspace,spawn,wofi-shutdown"

            # Brightness
            # ",XF86MonBrightnessUp,spawn,${brightnessctl} set 5%+"
            # ",XF86MonBrightnessDown,spawn,${brightnessctl} set 5%-"

            # # Volume
            # ",XF86AudioRaiseVolume,spawn,${pactl} set-sink-volume @DEFAULT_SINK@ +5%"
            # ",XF86AudioLowerVolume,spawn,${pactl} set-sink-volume @DEFAULT_SINK@ -5%"
            # ",XF86AudioMute,spawn,${pactl} set-sink-mute @DEFAULT_SINK@ toggle"
            # "SHIFT,XF86AudioMute,spawn,${pactl} set-source-mute @DEFAULT_SOURCE@ toggle"
            # ",XF86AudioMicMute,spawn,${pactl} set-source-mute @DEFAULT_SOURCE@ toggle"

            # # Media
            # ",XF86AudioNext,spawn,playerctl next"
            # ",XF86AudioPrev,spawn,playerctl previous"
            # ",XF86AudioPlay,spawn,playerctl play-pause"
            # ",XF86AudioStop,spawn,playerctl stop"

            # Screenshots
            # ",Print,spawn,${grimblast} --notify copy output"
            # "SHIFT,Print,spawn,${grimblast} --notify copy active"
            # "CTRL,Print,spawn,${grimblast} --notify copy screen"
            # "${mod},Print,spawn,${grimblast} --notify copy window"
            # "ALT,Print,spawn,${grimblast} --freeze --notify copy area"
            # "${mod}+SHIFT,p,spawn,${grimblast} --freeze --notify copy area"

            # Lock screen
            # ",XF86Launch5,spawn,swaylock -S"
            # "${mod},backspace,spawn,swaylock -S"
          ]
          # Workspace switching
          ++ (map (n: "${mod},${n},view,${n},0") workspaces)
           # Move window to workspace
          ++ (map (n: "${mod}+SHIFT,${n},tag,${n},0") workspaces)
          # Focus direction (hjkl + arrows)
          ++ (lib.mapAttrsToList (key: dir: "${mod},${key},focusdir,${dir}") directions)
          # Swap windows
          ++ (lib.mapAttrsToList (key: dir: "${mod}+SHIFT,${key},exchange_client,${dir}") directions)
          # Focus monitor
          ++ (lib.mapAttrsToList (key: dir: "${mod}+CTRL,${key},focusmonitor,${dir}") directions)
          # Move window to other monitor
          ++ (lib.mapAttrsToList (key: dir: "${mod}+CTRL+SHIFT,${key},sendtomonitor,${dir}") directions)
          # Move workspace to other monitor
          ++ (lib.mapAttrsToList (key: dir: "${mod}+ALT,${key},moveworkspacetomonitor,${dir}") directions);

        mousebind = [
          "${mod},btn_left,moveresize,curmove"
          "${mod},btn_right,moveresize,curresize"
        ];

        # Passthrough keymode
        keymode.passthrough.bind = [
          "${mod},p,setkeymode,default"
        ];
      };
    };
  };
}
