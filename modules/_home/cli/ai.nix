{ config, pkgs, lib, ... }:
{
  features.impermanence.directories = [
    ".config/fabric"
    ".local/share/oterm"
    ".cache/huggingface"
    ".config/opencode"
    ".local/share/opencode"
    ".local/state/opencode"
    ".config/claude"
    ".claude"
  ];

  features.impermanence.directories_cache = [ ".claude/Nextcloud" ];

  home.activation.createClaudeConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ ! -f "${config.xdg.configHome}/claude/config.json" ]; then
      mkdir -p "${config.xdg.configHome}/claude"
      echo '{}' > "${config.xdg.configHome}/claude/config.json"
    fi
  '';

  home.file.".claude.json".source =
    config.lib.file.mkOutOfStoreSymlink "${config.xdg.configHome}/claude/config.json";

  home.packages = with pkgs; [
    unstable.opencode
    fabric-ai
    aider-chat
    unstable.codex
  ];
}
