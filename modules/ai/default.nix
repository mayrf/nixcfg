{ inputs, ... }:
{
  flake.modules.homeManager.ai =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      features.impermanence.directories = [
        ".config/fabric"
        ".local/share/oterm"
        ".cache/huggingface"
        ".config/opencode"
        ".local/share/opencode"
        ".local/state/opencode"
      ];


      home.packages = with pkgs; [
        unstable.opencode
        # inputs.opencode.packages.${pkgs.stdenv.hostPlatform.system}.default
        fabric-ai
        aider-chat
        unstable.codex
      ];
    };
}
