{ pkgs, config, lib, ... }:
# options.my.gitRepo.path = lib.mkOption {
#   type = lib.types.str;
#   description = "Path to the Git repository.";
# };

let path = "/home/mayrf/.config/nixcfg";
in {
  systemd.tmpfiles.rules = [
    "d  /home/mayrf/.config/ 0755 mayrf users -"
    "d  /home/mayrf/.config/nixcfg 0755 mayrf users -"
  ];
  systemd.services.ensure-git-repo = {
    description = "Ensure Git repository exists";
    after = [ "network-online.target" ]; # Ensure network is up
    wants = [ "network-online.target" ];
    unitConfig = { StartLimitIntervalSec = 0; };
    serviceConfig = {
      Type = "oneshot";
      User = "mayrf"; # Or a specific user
      Group = "users";
      WorkingDirectory = path;
      ExecStart = "${pkgs.git}/bin/git clone https://github.com/mayrf/nixcfg .";
      #Only run ExecStart if a .git folder does NOT exist
      # ConditionPathExists="!${config.my.gitRepo.path}/.git";
      ConditionPathExists = "!${path}/.git";
    };
    wantedBy = [ "multi-user.target" ];
  };
}
