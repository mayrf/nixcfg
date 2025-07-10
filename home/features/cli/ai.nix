{ config, lib, pkgs, hostSpec, ... }:
with lib;
let
  cfg = config.features.cli.ai;
  # opencode-latest = pkgs.unstable.opencode.overrideAttrs (oldAttrs: rec {
  #   version = "0.1.183";
  #   subPackages = [ "cmd/opencode" ];
  #   src = pkgs.fetchFromGitHub {
  #     owner = "sst";
  #     repo = "opencode";
  #     tag = "v${version}";
  #     hash = "sha256-A4XM3K50lr/LcEhFhLNs5SX3ysnP5fCqD5aW1OXJSi8=";
  #   } + "/packages/tui";
  #   vendorHash = "sha256-/gDzGBCRapDBpEV9x1pB3QsOX9yua2RFY3i4OAjaIqc=";
  #   ldflags = [
  #     "-s"
  #     "-w"
  #     # "-X github.com/sst/opencode/packages/tui/internal/version.Version=${version}"
  #     # "-X github.com/sst/opencode/packages/tui/cmd/opencode/main.Version=${version}"
  #     "-X github.com/sst/opencode/internal/version.Version=${version}"
  #     # "-X main.Version=${version}"

  #   ];
  #   # nativeCheckInputs = [  ];
  #   # nativeCheckInputs = [ versionCheckHook ];
  #   # vendorHash = null;
  #   # vendorHash =  pkgs.lib.fakeHash;
  #   # subPackages = [ "packages/tui" ];
  #   # subPackages = [ "packages/tui/cmd/opencode" ];
  #   # modRoot = ["packages/tui"];
  # });
in {
  options.features.cli.ai.enable = mkEnableOption "enable ai cli programs";

  config = mkIf cfg.enable {

    features.impermanence.directories =
      [ ".config/fabric" ".local/share/oterm" ".cache/huggingface" ];
    home.packages = with pkgs; [
      unstable.opencode
      oterm
      fabric-ai
      # stable.aider-chat
      aider-chat
    ];
  };
}
