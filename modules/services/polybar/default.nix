{ config, pkgs, ... }: 
{
  #imports = [
  #  ./colors.nix
  #  ./modules.nix
  #];
  services.polybar = {
    enable = true;
    script = ''
      polybar main &
    '';
    config = {
      "colors" = {
	background = "#00";
	background-alt = "#3306080";
	primary = "#fff";
	alert = "#f7768e";
      };
      "bar/main" = {
	bottom = "false";

	background = "\${colors.background}";
	border-size = 0;
	border-color = "#00000000";

	module-margin-left  = 1;
	module-margin-right = 1;
	radius = "0.0";
	padding-left  = "2";


	font-0 = "JetBrainsMono Nerd Font:style=bold:size=10;2";

	modules-left   = "bspwm xwindow";
	modules-right   = "wlan";

	wm-restack = "bspwm";

      };
      "module/xwindow" ={
        type = "internal/xwindow";
	label = "%title:0:30:...%";
      };
      "module/bspwm" ={
        type = "internal/bspwm";
	#pin-workspaces = "true";
	pin-workspaces = "false";

	label-focused = "%index%";
	label-focused-backgroud = "\${colors.background-alt}";
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
	#format-connected = "%{A1:networkmanager_dmenu:} <label-connected>%{A}"; 
	#label-connected = "%{A1:wifimenu:}%essid%%{A}";
	#label-connected-padding = 1;

	#format-disconnected = <label-disconnected>
	#format-disconnected-padding = 1
	#label-disconnected = %{A1:wifimenu:}%{A}
	#label-disconnected-foreground = ${colors.overlay0}
	#label-disconnected-padding = 1
      };
      #"settings" = {
      # screenchange-reload = "true";
      #};
    };
  };
}
