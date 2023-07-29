{ config, options, lib, pkgs, ... }:

with lib;
let
  name = "";
  maildir = "/home/mayrf/Maildir";
  email = "";
  notmuchrc = "/home/mayrf/.config/notmuch/notmuchrc";
in
{
  programs = {
    mu.enable = true;
    msmtp.enable = true;
    mbsync.enable = true;
  };

  services = {
    mbsync = {
      enable = true;
      frequency = "*:0/15";
      preExec = "${pkgs.isync}/bin/mbsync -Ha";
      postExec = "${pkgs.mu}/bin/mu index -m ${maildir}";
    };
  };
  accounts.email = {
    maildirBasePath = "${maildir}";
    accounts = {
      hotmail = {
        address = "${email}";
        userName = "${email}";
        flavor = "outlook.office365.com";
        passwordCommand = "echo Password";
        primary = true;
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = [ "*" "[Hotmail]*" ]; # "[Gmail]/Sent Mail" ];
        };
        realName = "${name}";
      };
    };
  };
}
