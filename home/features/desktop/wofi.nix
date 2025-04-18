{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.wofi;
in {
  options.features.desktop.wofi.enable = mkEnableOption "wofi config";

  config = mkIf cfg.enable {
    programs.wofi = {
      enable = true;
      package = pkgs.wofi.overrideAttrs (oa: {
        patches = (oa.patches or [ ]) ++ [
          ./wofi-run-shell.patch # Fix for https://todo.sr.ht/~scoopta/wofi/174
        ];
      });
      settings = {
        image_size = 48;
        allow_images = true;
        insensitive = true;
        run-always_parse_args = true;
        run-cache_file = "/dev/null";
        run-exec_search = true;
      };
    };

    home.packages = let inherit (config.programs.password-store) package enable;
    in lib.optional enable (pkgs.pass-wofi.override { pass = package; });
  };
}
