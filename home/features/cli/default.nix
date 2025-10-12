{ pkgs, ... }: {
  imports = [
    ./zsh.nix
    ./fzf.nix
    ./ai.nix
    ./k8s.nix
    ./media.nix
    ./development.nix
    ./yazi.nix
    ./lf
    ./git
    ./scripts
    ./sops.nix
    ./syncthing.nix
  ];
  programs.zoxide = { enable = true; };
  programs.eza = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
    extraOptions = [ "-l" "--icons" "--git" "-a" ];
  };
  programs.bat = { enable = true; };

  home.packages = with pkgs; [
    coreutils
    fd
    htop
    # httpie
    btop
    jq
    procs
    ripgrep
    tldr
    zip
  ];
}
