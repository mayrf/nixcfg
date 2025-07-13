{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.features.cli.ai;
  opencode-latest = pkgs.unstable.opencode.overrideAttrs (oldAttrs: rec {
    version = "0.2.33";
    src = pkgs.fetchFromGitHub {
      owner = "sst";
      repo = "opencode";
      tag = "v0.2.33";
      hash =
        "sha256-l/V9YHwuIPN73ieMT++enL1O5vecA9L0qBDGr8eRVxY="; # You'll need to update this hash
    };
    tui = oldAttrs.tui.overrideAttrs (tuiOldAttrs: {
      vendorHash =
        "sha256-0vf4fOk32BLF9/904W8g+5m0vpe6i6tUFRXqDHVcMIQ"; # or vendorHash = "";
    });

    node_modules = oldAttrs.node_modules.overrideAttrs (node_modulesOldAttrs: {
      outputHash =
        "sha256-1ZxetDrrRdNNOfDOW2uMwMwpEs5S3BLF+SejWcRdtik="; # or vendorHash = "";
    });
  });
in {
  options.features.cli.ai.enable = mkEnableOption "enable ai cli programs";

  config = mkIf cfg.enable {

    features.impermanence.directories = [
      ".config/fabric"
      ".local/share/oterm"
      ".cache/huggingface"
      ".config/opencode"
      ".local/share/opencode"
      ".local/state/opencode"
      # ".cache/opencode"
    ];
    home.packages = with pkgs; [
      # unstable.opencode
      opencode-latest
      oterm
      fabric-ai
      # stable.aider-chat
      aider-chat
    ];
  };
}
