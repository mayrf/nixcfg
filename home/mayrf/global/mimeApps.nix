{ config, lib, pkgs, ... }:

{
  xdg.mimeApps.defaultApplications = {
    # "application/pdf" = "org.pwmt.zathura.desktop";
    "application/pdf" = "org.pwmt.zathura.desktop";
    "video/x-msvideo" = "vlc.desktop";
  };
}
