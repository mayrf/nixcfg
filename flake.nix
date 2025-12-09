{
  description = "mayrf's NixOs configuration";

  inputs = {
    ########## versioned inputs ##########
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";

    home-manager.url = "github:nix-community/home-manager";
    # home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    stylix.url = "github:danth/stylix";
    # stylix.url = "github:nix-community/stylix/release-25.11";
    stylix.inputs.nixpkgs.follows = "nixpkgs";

    nixvim.url = "github:nix-community/nixvim";
    # nixvim.url = "github:nix-community/nixvim/nixos-25.11";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    ########## unversioned inputs ##########
    dotfiles-private = {
      url = "git+ssh://git@codeberg.org/mayrf/dotfiles-private.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nixos-cli.url = "github:nix-community/nixos-cli";
    nix-colors.url = "github:misterio77/nix-colors";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-secrets = {
      url = "git+ssh://git@codeberg.org/mayrf/nix-secrets.git";
      flake = false;
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence.url = "github:nix-community/impermanence/home-manager-v2";
    hyprland.url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    winapps = {
      url = "github:winapps-org/winapps";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      # IMPORTANT: we're using "libgbm" and is only available in unstable so ensure
      # to have it up-to-date or simply don't specify the nixpkgs input  
      # inputs.nixpkgs.follows = "nixpkgs";
    };
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nvf.url = "github:notashelf/nvf";
    nvf.inputs.nixpkgs.follows = "nixpkgs";

    # set path to your cloned posimacs repo, which is a nix flake
    dotemacs = {
      url = "path:/home/mayrf/.config/dotemacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      forAllSystems = inputs.nixpkgs.lib.genAttrs [ "x86_64-linux" ];
      myLib = import ./myLib/default.nix {
        inherit inputs;
        lib = inputs.nixpkgs.lib;
      };
    in {
      packages = forAllSystems (system:
        (import ./pkgs nixpkgs.legacyPackages.${system})
        // (if system == "x86_64-linux" then {
          my-neovim = (inputs.nvf.lib.neovimConfiguration {
            pkgs = nixpkgs.legacyPackages.x86_64-linux;
            modules = [ ./pkgs/nvf_module ];
          }).neovim;
        } else
          { }));
      templates = import ./templates;
      overlays = import ./overlays { inherit inputs; };
      nixosConfigurations = {
        radium = myLib.mkSystem "radium" { nixosPath = ./hosts/radium; };
        yttrium = myLib.mkSystem "yttrium" { nixosPath = ./hosts/yttrium; };
        helium = myLib.mkSystem "helium" { nixosPath = ./hosts/helium; };
        kalium = myLib.mkSystem "kalium" { nixosPath = ./hosts/kalium; };
        valium = myLib.mkSystem "valium" { nixosPath = ./hosts/valium; };
      };
    };
}
