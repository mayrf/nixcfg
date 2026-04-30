{ ... }:
{
  flake.modules.homeManager.email =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [ gnupg offlineimap oauth2ms isync cyrus-sasl-xoauth2 ];
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
          postExec = "${pkgs.mu}/bin/mu index -m /home/mayrf/Maildir";
        };
      };
      accounts.email = {
        maildirBasePath = "/home/mayrf/Maildir";
        accounts = {
          hotmail = {
            address = "";
            userName = "";
            flavor = "outlook.office365.com";
            passwordCommand = "echo Password";
            primary = true;
            mbsync = {
              enable = true;
              create = "both";
              expunge = "both";
              patterns = [ "*" "[Hotmail]*" ];
            };
            realName = "";
          };
        };
      };
    };
}
