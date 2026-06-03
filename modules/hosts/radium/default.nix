{ inputs, self, ... }:
let
  specialArgs = {
    outputs = inputs.self.outputs;
    inherit inputs;
  };
in
{
  flake.modules.homeManager.radium =
    { inputs, pkgs, ... }:
    {
      imports = [
        self.modules.homeManager.hmImpermanence
        self.modules.homeManager.ensureSecretsRepo
        self.modules.homeManager.ensurePrivateConfigRepo
        self.modules.homeManager.ensureConfigRepo
        self.modules.homeManager.zsh
        self.modules.homeManager.fzf
        self.modules.homeManager.ai
        self.modules.homeManager.development
        self.modules.homeManager.k8s
        self.modules.homeManager.lf
        self.modules.homeManager.git
        self.modules.homeManager.scripts
        self.modules.homeManager.hmSops
        self.modules.homeManager.yazi
        self.modules.homeManager.syncthing
        self.modules.homeManager.emacs
        self.modules.homeManager.nvim
        self.modules.homeManager.zed
        inputs.dotfiles-private.modules.homeManager.radium
      ];

      home.packages = with pkgs; [
        mariadb
        grafana-alloy
        firefox
        camunda-modeler
        uv
      ];

      home.shellAliases = {
        cme = "uvx confluence-markdown-exporter";
      };
    };

  flake.modules.nixos.radium =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      imports = [
        self.modules.nixos.base
        self.modules.nixos.general
        self.modules.nixos.common
        self.modules.nixos.impermanence
        self.modules.nixos.emacs
        self.modules.nixos.claude
        self.modules.nixos.sops
        self.modules.nixos.docker
        ./_hardware-configuration.nix
        inputs.dotfiles-private.modules.nixos.radium
      ];

      host = {
        username = "mayrf";
      };
      networking.hostName = "radium";

      programs.nix-ld.enable = true;
      programs.nix-ld.libraries = with pkgs; [
        chromium
        libglibutil
      ];
      environment.systemPackages = with pkgs; [
        wsl-vpnkit
        xmodmap
      ];
      networking.networkmanager.enable = lib.mkForce false;

      systemd.user.services.ssh-proxy = {
        description = "SSH SOCKS Proxy";
        after = [ "network.target" ];
        wantedBy = [ "default.target" ];
        serviceConfig = {
          ExecStart = "${pkgs.openssh}/bin/ssh -N -D 1080 hollama";
          Restart = "on-failure";
          RestartSec = "5s";
        };
      };

      home-manager.users.${config.host.username}.imports = [
        self.modules.homeManager.radium
      ];
    };

  flake.nixosConfigurations.radium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules = [ self.modules.nixos.radium ];
  };
}
