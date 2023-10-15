{ config, lib, pkgs, ... }:

{
  xdg.mimeApps.defaultApplications = {
    "application/pdf" = "org.pwmt.zathura.desktop";
    "video/x-msvideo" = "vlc.desktop";
    "x-scheme-handler/postman" = "Postman.desktop";
    "x-scheme-handler/msteams" = "teams.desktop";
    "x-scheme-handler/http" = "librewolf.desktop";
    "x-scheme-handler/https" = "librewolf.desktop";
  };
}
# ls -l /run/current-system/sw/share/applications
