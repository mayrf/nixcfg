{ ... }:
{
  flake.modules.nixos.open-webui =
    { pkgs, ... }:
    {
      services.open-webui = {
        enable = true;
        package = pkgs.stable.open-webui;
        stateDir = "/var/lib/open-webui";
        port = 8080;
        environment = {
          OLLAMA_API_BASE_URL = "http://127.0.0.1:11434";
          WEBUI_AUTH = "False";
        };
      };
    };
}
