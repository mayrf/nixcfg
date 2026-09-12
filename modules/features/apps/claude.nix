{ inputs, self, ... }:
{
  flake.modules.nixos.claude =
    { config, ... }:
    {
      nixpkgs.overlays = [ inputs.claude-code.overlays.default ];

      home-manager.users.${config.host.username}.imports = [
        self.modules.homeManager.claude
      ];
    };
  flake.modules.homeManager.claude =
    { inputs, config, pkgs, ... }:
    {
      features.impermanence.directories = [
        ".config/claude"
        ".claude"
      ];
      home.sessionVariables.CLAUDE_CONFIG_DIR = "${config.xdg.configHome}/claude";
      home.packages = [ pkgs.claude-code pkgs.nodejs_26 ];
    };
}
