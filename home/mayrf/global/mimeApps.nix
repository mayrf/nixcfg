{ config, lib, pkgs, ... }:

{
  # xdg.mimeApps.addedAssociations = {
  #   "x-scheme-handler/mailto" = "userapp-Thunderbird-08TXD2.desktop";
  #   "x-scheme-handler/mid" = "userapp-Thunderbird-08TXD2.desktop";
  # };
  xdg.mimeApps.defaultApplications = {
    "application/pdf" = "org.pwmt.zathura.desktop";
    "video/x-msvideo" = "vlc.desktop";
    "x-scheme-handler/postman" = "Postman.desktop";
    "x-scheme-handler/msteams" = "teams.desktop";
    "x-scheme-handler/http" = "librewolf.desktop";
    "x-scheme-handler/https" = "librewolf.desktop";
    "x-scheme-handler/mailto" = "userapp-Thunderbird-08TXD2.desktop";
    "message/rfc822" = "userapp-Thunderbird-08TXD2.desktop";
    "x-scheme-handler/mid" = "userapp-Thunderbird-08TXD2.desktop";
  };
}
# ls -l /run/current-system/sw/share/applications
