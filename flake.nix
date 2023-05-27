{
  description = "My personal nixOs flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wlroots = {
      url = "gitlab:wlroots/wlroots?host=gitlab.freedesktop.org";
      flake = false;
    };

    hyprland-protocols = {
      url = "github:hyprwm/hyprland-protocols";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xdph = {
      url = "github:hyprwm/xdg-desktop-portal-hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hyprland-protocols.follows = "hyprland-protocols";
    };
  };

  outputs = { self, nixpkgs, home-manager, wlroots, hyprland-protocols, xdph }:
    let user = "mayrf";
    in {
      nixosConfigurations = ( # NixOS configurations
        import ./hosts { # Imports ./hosts/default.nix
          inherit nixpkgs user home-manager wlroots hyprland-protocols
            xdph; # Also inherit home-manager so it does not need to be defined here.
        });
    };
}
