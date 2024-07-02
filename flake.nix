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
      url = "git+ssh://git@gitlab.com/mayrf/nix-secrets.git?ref=main&shallow=1";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs:
    let
      # inherit (self) outputs;
      aidLib = import ./aidLib/default.nix { inherit inputs; };
    in with aidLib; {
      homemanagermodules = import ./modules/home-manager;
      nixosmodules = import ./modules/nixos;
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
