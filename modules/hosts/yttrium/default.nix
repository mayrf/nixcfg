{ inputs, self, ... }:
let
  specialArgs = {
    outputs = inputs.self.outputs;
    inherit inputs;
    inherit (inputs.dotfiles-private) private;
  };
in
{
  flake.modules.homeManager.yttrium = {
    imports = [ ../../../home/mayrf/yttrium.nix ];
  };

  flake.modules.nixos.yttrium =
    { config, ... }:
    {
      imports = [
        self.modules.nixos.base
        self.modules.nixos.common
        self.modules.nixos.impermanence
        self.modules.nixos.commonModules
        self.modules.nixos.emacs
        self.modules.nixos.claude
        self.modules.nixos.sops
        self.modules.nixos.bluetooth
        self.modules.nixos.docker
        self.modules.nixos.flatpak
        self.modules.nixos.gaming
        self.modules.nixos.kanata
        self.modules.nixos.open-webui
        self.modules.nixos.pipewire
        self.modules.nixos.printing
        self.modules.nixos.theming
        self.modules.nixos.virtualisation
        self.modules.nixos.winapps
        ../../../hosts/yttrium
      ];
      persistence.enable = true;
      persistence.user = config.preferences.user.name;
      home-manager.users.${config.hostSpec.username}.imports = [
        self.modules.homeManager.yttrium
      ];
    };

  flake.nixosConfigurations.yttrium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules = [ self.modules.nixos.yttrium ];
  };
}
