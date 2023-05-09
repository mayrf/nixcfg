{ config, pkgs, ... }: {
  xsession.windowManager.bspwm = {
    enable = true;
    extraConfigEarly = ''
      bspc monitor -d I II III IV V VI VII VIII IX X
      bspc config pointer_motion_interval 40
      bspc config border_width 0
      bspc config window_gap 12
      bspc config split_ratio 0.50
      bspc config borderless_monocle true
      bspc config gapless_monocle true
    '';
    extraConfig = ''
      pkill polybar 
      polybar &
      #feh --bg-fill $HOME/.background-image &
      #xsetroot -cursor_name left_ptr &
    '';
  };
}
