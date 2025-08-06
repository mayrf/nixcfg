{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.features.cli.ai;
  opencode-latest = pkgs.unstable.opencode.overrideAttrs (oldAttrs: rec {
    version = "0.3.130";
    src = pkgs.fetchFromGitHub {
      owner = "sst";
      repo = "opencode";
      tag = "v${version}";
      hash = "sha256-/FWvHekyAM9U5WLptAr2YbcMOZa/twjucSUnlqfu1Y4="; # You'll need to update this hash
      # hash = "";
    };
    tui = oldAttrs.tui.overrideAttrs (tuiOldAttrs: {
      vendorHash =
        "sha256-qsOL6gsZwEm7YcYO/zoyJAnVmciCjPYqPavV77psybU="; # or vendorHash = "";

      # vendorHash = "";
    });

    node_modules = oldAttrs.node_modules.overrideAttrs (node_modulesOldAttrs: {

      # outputHash =
      #   ""; # or vendorHash = "";
      outputHash =
        "sha256-oZa8O0iK5uSJjl6fOdnjqjIuG//ihrj4six3FUdfob8="; # or vendorHash = "";
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
      unstable.codex
    ];
  };
}
