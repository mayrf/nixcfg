{ inputs, ... }:
{
  flake.modules.nixos.nix =
    { pkgs, ... }:
    {
      imports = [
        inputs.nix-index-database.nixosModules.nix-index
      ];
      programs.nix-index-database.comma.enable = true;
      programs.command-not-found.enable = false;
      # for home-manager, use programs.bash.initExtra instead
      programs.zsh.interactiveShellInit = ''
        source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
      '';

      programs.direnv = {
        enable = true;
        silent = false;
        loadInNixShell = true;
        direnvrcExtra = "";
        nix-direnv = {
          enable = true;
        };
      };

      nix.settings.experimental-features = [
        "nix-command"
        "flakes"
      ];
      programs.nix-ld.enable = true;
      nixpkgs.config.allowUnfree = true;

      environment.systemPackages = with pkgs; [
        # Nix tooling
        nil
        nixd
        statix
        nix-inspect
      ];
    };
}
