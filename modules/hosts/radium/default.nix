{ inputs, self, ... }:
let
  specialArgs = {
    outputs = inputs.self.outputs;
    inherit inputs;
    inherit (inputs.dotfiles-private) private;
  };
in
{
  flake.modules.homeManager.radium =
    { inputs, pkgs, ... }:
    {
      imports = [
        inputs.self.modules.homeManager.hmImpermanence
        inputs.self.modules.homeManager.ensureSecretsRepo
        inputs.self.modules.homeManager.ensurePrivateConfigRepo
        inputs.self.modules.homeManager.ensureConfigRepo
        inputs.self.modules.homeManager.zsh
        inputs.self.modules.homeManager.fzf
        inputs.self.modules.homeManager.ai
        inputs.self.modules.homeManager.development
        inputs.self.modules.homeManager.k8s
        inputs.self.modules.homeManager.lf
        inputs.self.modules.homeManager.git
        inputs.self.modules.homeManager.scripts
        inputs.self.modules.homeManager.hmSops
        inputs.self.modules.homeManager.yazi
        inputs.self.modules.homeManager.syncthing
        inputs.self.modules.homeManager.emacs
        inputs.self.modules.homeManager.nvim
        inputs.self.modules.homeManager.zed
        inputs.dotfiles-private.outputs.homeManagerModules
      ];

      colorscheme = inputs.nix-colors.colorschemes.woodland;

      home.packages = with pkgs; [
        mariadb
        grafana-alloy
        firefox
        camunda-modeler
      ];

      features.private.work.enable = true;
    };

  flake.modules.nixos.radium =
    { config, lib, pkgs, ... }:
    {
      imports = [
        self.modules.nixos.base
        self.modules.nixos.common
        self.modules.nixos.impermanence
        self.modules.nixos.emacs
        self.modules.nixos.claude
        self.modules.nixos.sops
        self.modules.nixos.docker
        ./_hardware-configuration.nix
        inputs.dotfiles-private.outputs.nixosModules
      ];

      host = {
        username = "mayrf";
      };
      networking.hostName = "radium";

      features.private = {
        workProxies.enable = true;
        work.enable = true;
      };

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

      services.dnsmasq = {
        enable = true;
        settings = {
          address = [ "/accounts.hobex.io/217.196.147.21" ];
          listen-address = "127.0.0.1";
          bind-interfaces = true;
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
