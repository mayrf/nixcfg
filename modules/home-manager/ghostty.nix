{ config, pkgs, lib, ... }:
with lib;
let cfg = config.myGhostty;
in {
  options.myGhostty = { enable = mkEnableOption "my ghostty config"; };
  config = mkIf cfg.enable {
    programs.ghostty = {
      enable = true;
      enableZshIntegration = true;
      installVimSyntax = true;
      # themes = { };
      settings = {
        gtk-titlebar = false;
        shell-integration = "zsh";
      };

    };
    # services.open-webui = {
    #   enable = true;
    #   package = stable.open-webui;
    #   stateDir = "/var/lib/open-webui";
    #   port = 8080;
    #   environment = {
    #     OLLAMA_API_BASE_URL = "http://127.0.0.1:11434";
    #     # Disable authentication
    #     WEBUI_AUTH = "False";
    #   };
    # };
  };
}
