{
  description = "mayrf's NixOs configuration";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
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
    stylix.url = "github:danth/stylix";

    # nix-secrets = {
    #   url = "git+ssh://git@github.com:mayrf/sops.git?ref=main&shallow=1";
    #   flake = false;
    # };
  };

  outputs = { ... }@inputs: {

    homeManagerModules = import ./modules/home-manager;
    templates = import ./templates;
    wallpapers = import ./home/mayrf/wallpapers;
    nixosConfigurations = let
      configs = [
        {
          user = "mayrf";
          host = "yttrium";
        }
        {
          user = "mayrf";
          host = "radium";
        }
      ];
      moduleImports = [
        inputs.sops-nix.nixosModules.sops
        inputs.home-manager.nixosModules.home-manager
        inputs.nixos-wsl.nixosModules.wsl
        inputs.stylix.nixosModules.stylix
      ];
    in builtins.listToAttrs (map (config: {
      name = "${config.host}";
      value = let
        user = "${config.user}";
        host = "${config.host}";

        pkgs-stable = import inputs.nixpkgs-stable {
          system = "x86_64-linux"; # System Architecture
          config.allowUnfree = true;
        };
      in inputs.nixpkgs.lib.nixosSystem {
        modules = [
          ./hosts/${host}
          {
            home-manager.extraSpecialArgs = {
              inherit inputs user host pkgs-stable;
            };
            home-manager.users.${user} = {
              imports = [ ./home/mayrf/${host}.nix ];
            };
          }
        ] ++ moduleImports;
        specialArgs = { inherit inputs user host pkgs-stable; };
      };
    }) configs) // {
      helium = let
        user = "mayrf";
        host = "helium";
        pkgs-stable = import inputs.nixpkgs-stable {
          system = "x86_64-linux"; # System Architecture
          config.allowUnfree = true;
        };
      in inputs.nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs user host pkgs-stable; };
        modules = [
          ./hosts/helium
          {
            home-manager.extraSpecialArgs = {
              inherit inputs user host pkgs-stable;
            };
            home-manager.users.mayrf = {
              imports = [ ./home/mayrf/helium.nix ];
            };
          }
        ] ++ moduleImports;
      };
    };

  };
}
