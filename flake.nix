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

    nix-secrets = {
      url = "git+ssh://git@github.com/mayrf/sops.git?ref=main&shallow=1";
      flake = false;
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence = { url = "github:nix-community/impermanence"; };
    hyprland.url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
  };

  outputs = { self, ... }@inputs:
    let
      forAllSystems = inputs.nixpkgs.lib.genAttrs [
        "x86_64-linux"
        #"aarch64-darwin"
      ];
      # inherit (self) outputs;
      configVars = import ./vars { inherit inputs; };
      aidLib = import ./aidLib/default.nix { inherit inputs configVars; };
    in with aidLib; {
      packages = forAllSystems (system:
        let pkgs = inputs.nixpkgs.legacyPackages.${system};
        in import ./pkgs { inherit pkgs; });
      homeManagerModules = import ./modules/home-manager;
      nixosModules = import ./modules/nixos;
      templates = import ./templates;
      nixosConfigurations = {
        radium = mkSystem {
          user = "mayrf";
          host = "radium";
          nixosPath = ./hosts/radium;
          homePath = ./home/mayrf/radium.nix;
        };
        yttrium = mkSystem {
          user = "mayrf";
          host = "yttrium";
          nixosPath = ./hosts/yttrium;
          homePath = ./home/mayrf/yttrium.nix;
        };
        helium = mkSystem {
          user = "mayrf";
          host = "helium";
          nixosPath = ./hosts/helium;
          homePath = ./home/mayrf/helium.nix;
        };
      };
    };
}
