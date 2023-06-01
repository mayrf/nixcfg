{ config, lib, pkgs, ... }:

{

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
        key <AC09> { [     l,   l, oacute,      Oacute      ]   };


        // make right alt altGr
        include "level3(ralt_switch)"
    };  '';
  # TODO set up cliphist
  home.packages = with pkgs; [ cliphist ];
  xdg.configFile."hypr/hyprland.conf".text = ''
    exec-once = wl-paste --type text --watch cliphist store #Stores only text data
    exec-once = wl-paste --type image --watch cliphist store #Stores only image data

    input {
      kb_layout = us
      kb_variant = altgr-intl
      # kb_options = compose:menu
      # kb_options = compose:menu,compose:ralt,level3:ralt_switch,swedish_letters_on_dvorak:sweletters
      kb_options = hungarian_letters:huletters
      # kb_options = compose:menu,compose:ralt,level3:ralt_switch
    }
    monitor=VGA-1, 1920x1080, 0x0, 1
    monitor=LVDS-1, 1366x768, 0x1080, 1
    monitor=,preferred,auto,1
    general {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more

        gaps_in = 5
        gaps_out = 20
        border_size = 2
        col.active_border = rgba(33ccffee) rgba(00ff99ee) 45deg
        col.inactive_border = rgba(595959aa)

        layout = master
    }

    $mainMod = SUPER
    workspace=VGA-1,1
    bind = $mainMod, Q, killactive,
    bind = $mainMod SHIFT, Backspace, exit,
    bind = $mainMod, Return, exec,${pkgs.foot}/bin/foot
    # bind = $mainMod, W, $BROWSER
    bind = $mainMod, D, exec, ${pkgs.wofi}/bin/wofi --show drun
    bind = $mainMod, V, exec, cliphist list | wofi -dmenu | cliphist decode | wl-copy
    bind = $mainMod, Space, layoutmsg,swapwithmaster
    bind = $mainMod, J, layoutmsg, cyclenext
    bind = $mainMod, K, layoutmsg, cycleprev
    bind = $mainMod SHIFT, R, exec, ${pkgs.hyprland}/bin/hyprctl reload
    bind = $mainMod, T, exec,${pkgs.emacs}/bin/emacsclient -c
    bind = $mainMod, F, fullscreen,

    # Switch workspaces with mainMod + [0-9]
    bind = $mainMod, 1, workspace, 1
    bind = $mainMod, 2, workspace, 2
    bind = $mainMod, 3, workspace, 3
    bind = $mainMod, 4, workspace, 4
    bind = $mainMod, 5, workspace, 5
    bind = $mainMod, 6, workspace, 6
    bind = $mainMod, 7, workspace, 7
    bind = $mainMod, 8, workspace, 8
    bind = $mainMod, 9, workspace, 9
    bind = $mainMod, 0, workspace, 10
    bind = $mainMod, right, workspace,+1
    bind = $mainMod, left, workspace,-1

    # Move active window to a workspace with mainMod + SHIFT + [0-9]
    bind = $mainMod SHIFT, 1, movetoworkspace, 1
    bind = $mainMod SHIFT, 2, movetoworkspace, 2
    bind = $mainMod SHIFT, 3, movetoworkspace, 3
    bind = $mainMod SHIFT, 4, movetoworkspace, 4
    bind = $mainMod SHIFT, 5, movetoworkspace, 5
    bind = $mainMod SHIFT, 6, movetoworkspace, 6
    bind = $mainMod SHIFT, 7, movetoworkspace, 7
    bind = $mainMod SHIFT, 8, movetoworkspace, 8
    bind = $mainMod SHIFT, 9, movetoworkspace, 9
    bind = $mainMod SHIFT, 0, movetoworkspace, 10

  '';
}
