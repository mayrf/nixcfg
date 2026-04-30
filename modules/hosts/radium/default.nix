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
        ../../_home/general/impermanence.nix
        ../../_home/general/ensure-secrets-repo.nix
        ../../_home/general/ensure-private-config-repo.nix
        ../../_home/general/ensure-config-repo.nix
        ../../_home/cli/zsh.nix
        ../../_home/cli/fzf.nix
        ../../_home/cli/ai.nix
        ../../_home/cli/development.nix
        ../../_home/cli/k8s.nix
        ../../_home/cli/lf
        ../../_home/cli/git
        ../../_home/cli/scripts
        ../../_home/cli/sops.nix
        ../../_home/cli/yazi.nix
        ../../_home/cli/syncthing.nix
        ../../_home/editor/emacs
        ../../_home/editor/nvim.nix
        ../../_home/editor/zed.nix
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
        self.modules.nixos.commonModules
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
