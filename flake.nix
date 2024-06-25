{
  description = "My personal NixOs configuration";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";
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

    nix-colors.url = "github:misterio77/nix-colors";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    catppuccin.url = "github:catppuccin/nix";

    # https://github.com/nix-community/nixos-vscode-server
    vscode-server.url = "github:nix-community/nixos-vscode-server";

    # nix-secrets = {
    #   url = "git+ssh://git@github.com:mayrf/sops.git?ref=main&shallow=1";
    #   flake = false;
    # };
  };

  outputs = { self, nixpkgs, nixpkgs-stable, home-manager, ... }@inputs:
    let
      inherit (self) outputs;
      lib = nixpkgs.lib // home-manager.lib;
    in {
      inherit lib;

      homeManagerModules = import ./modules/home-manager;
      templates = import ./templates;
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

          pkgs-stable = import nixpkgs-stable {
            system = "x86_64-linux"; # System Architecture
            config.allowUnfree = true;
          };
        in lib.nixosSystem {
          modules = [
            ./hosts/${host}
            inputs.sops-nix.nixosModules.sops
            inputs.catppuccin.nixosModules.catppuccin
            home-manager.nixosModules.home-manager
            inputs.nixos-wsl.nixosModules.wsl
            inputs.vscode-server.nixosModules.default
            {
              home-manager.extraSpecialArgs = {
                inherit inputs outputs user host pkgs-stable;
              };
              home-manager.users.${user} = {
                imports = [
                  ./home/mayrf/${host}.nix
                  inputs.vscode-server.homeModules.default
                  inputs.catppuccin.homeManagerModules.catppuccin
                ];
              };
            }
          ];
          specialArgs = { inherit inputs outputs user host pkgs-stable; };
        };
      }) configs);

      darwinConfigurations = let
        user = "fmayr";
        host = "osmium";
      in {
        ${host} = inputs.darwin.lib.darwinSystem {
          modules = [
            ./hosts/osmium
            home-manager.darwinModules.home-manager
            {
              home-manager.extraSpecialArgs = {
                inherit inputs outputs user host;
              };
              home-manager.users.fmayr = {
                imports = [ ./home/mayrf/osmium.nix ];
              };
            }
          ];
          specialArgs = { inherit inputs outputs user host; };
        };
      };
    };
}
