{
  description = "mayrf's NixOs configuration";

  inputs = {
    dotfiles-private.url =
      "git+ssh://git@codeberg.org/mayrf/dotfiles-private.git";

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      # url = "github:nix-community/home-manager/release-24.11";
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
    stylix.inputs.nixpkgs.follows = "nixpkgs";

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
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixvim = {
      url = "github:nix-community/nixvim";
      # If using a stable channel you can use `url = "github:nix-community/nixvim/nixos-<version>"`
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      forAllSystems = inputs.nixpkgs.lib.genAttrs [
        "x86_64-linux"
        #"aarch64-darwin"
      ];
      # inherit (self) outputs;
      aidLib = import ./aidLib/default.nix {
        inherit inputs;
        lib = inputs.nixpkgs.lib;
      };
    in {
      packages =
        forAllSystems (system: import ./pkgs nixpkgs.legacyPackages.${system});

      # packages = forAllSystems (system:
      #   let pkgs = inputs.nixpkgs.legacyPackages.${system};
      #   in import ./pkgs { inherit pkgs; });
      overlays = import ./overlays { inherit inputs; };
      homeManagerModules = import ./modules/home-manager;
      nixosModules = import ./modules/nixos;
      templates = import ./templates;
      nixosConfigurations = {
        radium = aidLib.mkSystem "radium" { nixosPath = ./hosts/radium; };
        yttrium = aidLib.mkSystem "yttrium" { nixosPath = ./hosts/yttrium; };
        helium = aidLib.mkSystem "helium" { nixosPath = ./hosts/helium; };
        kalium = aidLib.mkSystem "kalium" { nixosPath = ./hosts/kalium; };
      };
    };
}
