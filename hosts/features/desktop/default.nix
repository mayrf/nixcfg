{ pkgs, ... }: {
  # imports = [
  # ];
  environment.systemPackages = with pkgs; [
    bluetuith
    libsForQt5.qt5.qtwayland
    # libsForQt5.polkit-kde-agent
    kdePackages.polkit-kde-agent-1
  ];
}
