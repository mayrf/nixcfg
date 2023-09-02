{ config, lib, pkgs, ... }:

{
  xdg.mimeApps.defaultApplications = {
    "application/pdf" = "org.pwmt.zathura.desktop";
    "video/x-msvideo" = "vlc.desktop";
    "x-scheme-handler/postman" = "Postman.desktop";
  };
}
