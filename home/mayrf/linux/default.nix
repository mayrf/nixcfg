{ lib, config, pkgs, hostSpec, ... }: {
  imports = [ ../global/programs.nix ./virtualisation.nix ./nextcloud.nix ];

  services = {
    gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-qt;
    };
  };
  stylix.targets.emacs.enable = false;

  home = {
    username = hostSpec.username;
    homeDirectory = lib.mkDefault "/home/${hostSpec.username}";
    stateVersion = lib.mkDefault "25.05";
    sessionPath = [ "$HOME/.local/bin" ];
  };

}
