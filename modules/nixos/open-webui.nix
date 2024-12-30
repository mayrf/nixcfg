{ config, pkgs, stable, lib, ... }:
with lib;
let cfg = config.mymodules.open-webui;
in {
  options.mymodules.open-webui = {
    enable = mkEnableOption "my open-webui config";
  };
  config = mkIf cfg.enable {
    services.open-webui = {
      enable = true;
      package = stable.open-webui;
      stateDir = "/var/lib/open-webui";
      port = 8080;
      environment = {
        OLLAMA_API_BASE_URL = "http://127.0.0.1:11434";
        # Disable authentication
        WEBUI_AUTH = "False";
      };
    };
  };
}
