{
  description = "My personal NixOs configuration";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      # MacOS Package Management
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";

    nix-colors.url = "github:misterio77/nix-colors";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      # inputs.flake-utils.follows = "flake-utils";
    };
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, nixpkgs-stable, home-manager, darwin, nixos-wsl
    , ... }@inputs:
    let
      inherit (self) outputs;
      lib = nixpkgs.lib // home-manager.lib;
    in {
      inherit lib;
      nixpkgs.overlays = [
        (import (builtins.fetchTarball {
          url =
            "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
        }))
      ];

      homeManagerModules = import ./modules/home-manager;
      templates = import ./templates;
      overlays = import ./overlays { inherit inputs outputs; };
      wallpapers = import ./home/mayrf/wallpapers;
      nixosConfigurations = let
        configs = [
          {
            user = "mayrf";
            host = "helium";
          }
          {
            user = "mayrf";
            host = "yttrium";
          }
          {
            user = "mayrf";
            host = "radium";
          }
        ];
      in builtins.listToAttrs (map (config: {
        name = "${config.host}";
        value = let
          user = "${config.user}";
          host = "${config.host}";

          # pkgs = import nixpkgs {
          #   # inherit system;
          #   config.allowUnfree = true; # Allow Proprietary Software
          # };

          stable = import nixpkgs-stable {
            system = "x86_64-linux"; # System Architecture
            config.allowUnfree = true;
          };
        in lib.nixosSystem {
          modules = [
            ./hosts/${host}
            home-manager.nixosModules.home-manager
            nixos-wsl.nixosModules.wsl
            {
              home-manager.extraSpecialArgs = {
                inherit inputs outputs user host stable;
              };
              home-manager.users.${user} = {
                imports = [ ./home/mayrf/${host}.nix ];
              };
            }
          ];
          specialArgs = { inherit inputs outputs user host stable; };
        };
      }) configs);

      darwinConfigurations = let
        user = "fmayr";
        host = "osmium";
      in {
        ${host} = darwin.lib.darwinSystem {
          modules = [
            ./hosts/osmium
            home-manager.darwinModules.home-manager
            {
              home-manager.extraSpecialArgs = {
                inherit inputs outputs user host;
              };
              home-manager.users.fmayr = {
                imports = [ ./home/mayrf/osmium.nix ];
                # home.stateVersion = "23.11";
              };
            }
          ];
          specialArgs = { inherit inputs outputs user host; };
        };
      };
    };
}
