{ self, inputs, ... }:
{

  # package = self'.packages.myNoctalia;
  perSystem =
    { pkgs, ... }:
    {
      packages.myNoctalia = inputs.wrapper-modules.wrappers.noctalia-shell.wrap {
        inherit pkgs; 

        # nix run nixpkgs#noctalia-shell ipc call state all > $FLAKE/modules/features/noctalia/noctalia.json
        settings = (builtins.fromJSON (builtins.readFile ./noctalia.json)).settings;
      };
    };
  flake.modules.nixos.noctalia = { pkgs, ... }: {
    imports = [
      inputs.noctalia.nixosModules.default
    ];

    programs.noctalia = {
      enable = true;
      # Enables NetworkManager, Bluetooth, UPower, and a power profile service.
      recommendedServices.enable = true;
    };
    

    nix.settings = {
      extra-substituters = [ "https://noctalia.cachix.org" ];
      extra-trusted-public-keys = [ "noctalia.cachix.org-1:pCOR47nnMEo5thcxNDtzWpOxNFQsBRglJzxWPp3dkU4=" ];
    };

  };


}
