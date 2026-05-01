{ inputs, ... }:
{
  flake.overlays = {
    additions = final: _prev: {
      auger = final.callPackage ../packages/_auger { };
      argocd-lovely-plugin = final.callPackage ../packages/_argocd-lovely-plugin { };
    };

    stable-packages = final: _prev: {
      stable = import inputs.nixpkgs-stable {
        system = final.stdenv.hostPlatform.system;
        config.allowUnfree = true;
        config.allowBroken = true;
      };
    };

    unstable-packages = final: _prev: {
      unstable = import inputs.nixpkgs-unstable {
        system = final.stdenv.hostPlatform.system;
        config.allowUnfree = true;
      };
    };
  };
}
