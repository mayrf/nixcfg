{
  description = "My personal NixOs configuration";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = { # MacOS Package Management
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

  };

  outputs = { self, nixpkgs, home-manager, darwin, ... }@inputs:
    let
      inherit (self) outputs;
      lib = nixpkgs.lib // home-manager.lib;
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      # system = [ "x86_64-linux" "aarch64-linux"  "aarch64-darwin"];

      pkgsFor = nixpkgs.legacyPackages;
      forEachSystem = f: lib.genAttrs systems (sys: f pkgsFor.${sys});
    in {
      inherit lib;

      homeManagerModules = import ./modules/home-manager;
      templates = import ./templates;
      overlays = import ./overlays { inherit inputs outputs; };
      wallpapers = import ./home/mayrf/wallpapers;

      # sudo nixos-rebuild switch --flake .#${host}
      nixosConfigurations = {
        # Personal laptop
        helium = let
          user = "mayrf";
          host = "helium";
        in lib.nixosSystem {
          modules = [
            ./hosts/helium
            home-manager.nixosModules.home-manager
            {
              home-manager.extraSpecialArgs = {
                inherit inputs outputs user host;
              };
              home-manager.users.mayrf = {
                imports = [ ./home/mayrf/helium.nix ];
              };
            }
          ];
          specialArgs = { inherit inputs outputs user host; };
        };

        # Work
        tellur = let
          user = "mayrf";
          host = "tellur";
        in lib.nixosSystem {
          modules = [
            ./hosts/tellur
            home-manager.nixosModules.home-manager
            {
              home-manager.extraSpecialArgs = {
                inherit inputs outputs user host;
              };
              home-manager.users.mayrf = {
                imports = [ ./home/mayrf/tellur.nix ];
              };
            }
          ];
          specialArgs = { inherit inputs outputs user host; };
        };

        #desktop
        yttrium = let
          user = "mayrf";
          host = "yttrium";
        in lib.nixosSystem {
          modules = [
            ./hosts/yttrium
            home-manager.nixosModules.home-manager
            {
              home-manager.extraSpecialArgs = {
                inherit inputs outputs user host;
              };
              home-manager.users.mayrf = {
                imports = [ ./home/mayrf/yttrium.nix ];
              };
            }
          ];
          specialArgs = { inherit inputs outputs user host; };
        };
      };

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
