{ inputs, self, ... }:
{
  # Minimal bootstrapping configuration for installing any host.
  # No secrets, no private repos, no home-manager.
  # Deploy one of these to a machine, then switch to the full config.
  flake.modules.nixos.installer =
    { config, lib, pkgs, ... }:
    {
      imports = [
        ../../_host-spec.nix
      ];

      host = {
        username = "mayrf";
        # required options not used by the installer — set to satisfy the type checker
        email = { };
        domain = "";
        userFullName = "";
        handle = "";
      };

      networking.hostName = lib.mkDefault "installer";
      system.stateVersion = "26.05";

      boot.initrd.systemd.enable = false;

      services.openssh = {
        enable = true;
        ports = [ 22 ];
        settings.PermitRootLogin = "yes";
      };

      # Allow sudo over SSH with yubikey
      security.pam = {
        sshAgentAuth.enable = true;
        services.sudo = {
          u2fAuth = true;
          sshAgentAuth = true;
        };
      };

      environment.systemPackages = with pkgs; [
        wget
        curl
        rsync
        vim
        git
      ];

      nix.settings = {
        experimental-features = [ "nix-command" "flakes" ];
        warn-dirty = false;
      };

      programs.ssh.extraConfig = ''
        Host *
          IdentityFile /persist/system/home/${config.host.username}/.ssh/id_ed25519
          IdentitiesOnly yes
      '';
    };

  flake.nixosConfigurations.installer-yttrium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      inputs.impermanence.nixosModules.impermanence
      inputs.disko.nixosModules.default
      self.modules.nixos.installer
      ../yttrium/_hardware-configuration.nix
      (import ../yttrium/_disko.nix { device = "/dev/nvme0n1"; })
      { networking.hostName = "yttrium"; }
    ];
  };

  flake.nixosConfigurations.installer-helium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      inputs.impermanence.nixosModules.impermanence
      inputs.disko.nixosModules.default
      self.modules.nixos.base
      self.modules.nixos.installer
      ../helium/_hardware-configuration.nix
      (import ../helium/_disko.nix { device = "/dev/sda"; })
      { networking.hostName = "helium"; }
    ];
  };

  flake.nixosConfigurations.installer-radium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      inputs.impermanence.nixosModules.impermanence
      inputs.nixos-wsl.nixosModules.wsl
      self.modules.nixos.installer
      ../radium/_hardware-configuration.nix
      { networking.hostName = "radium"; }
    ];
  };
}
