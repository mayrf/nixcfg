{
  description = "My personal NixOs configuration";

  inputs = {

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

  };

  outputs = { self, nixpkgs, home-manager, darwin, ... }@inputs:
    let
      inherit (self) outputs;
      lib = nixpkgs.lib // home-manager.lib;
    in {
      inherit lib;

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
        ];
      in builtins.listToAttrs (map (config: {
        name = "${config.host}";
        value = let
          user = "${config.user}";
          host = "${config.host}";
        in lib.nixosSystem {
          modules = [
            ./hosts/${host}
            home-manager.nixosModules.home-manager
            {
              home-manager.extraSpecialArgs = {
                inherit inputs outputs user host;
              };
              home-manager.users.${user} = {
                imports = [ ./home/mayrf/${host}.nix ];
              };
            }
          ];
          specialArgs = { inherit inputs outputs user host; };
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
