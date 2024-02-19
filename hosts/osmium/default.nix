{ pkgs, user, ... }: {
  # List packages installed in system profile. To search by name, run:
  imports = import (../../darwin/modules);
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    vim
    jetbrains.idea-community
    dbeaver
    azure-cli
    nixfmt
    # colima
    docker
    ollama
    rectangle
    docker
    docker-compose
    docker-machine
    tree
    jq
  ];

  homebrew = {
    enable = true;
    global.autoUpdate = true;
    brews = [ "lima" "colima" "koekeishiya/formulae/skhd" ];
    casks = [ "librewolf" ];
    # taps = [ "koekeishiya/formulae/skhd" ];
    # taps = [ "mac-vz/tap/macvz" ];
  };

  services.karabiner-elements.enable = true;
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";
  security.pam.enableSudoTouchIdAuth = true;

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina
  # programs.fish.enable = true;

  # Set Git commit hash for darwin-version.
  # system.configurationRevision = self.rev or self.dirtyRev or null;
  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "right";
  # yabai.enable = true;
  # skhd.enable = true;
  system.keyboard.remapCapsLockToEscape = true;
  system.keyboard.enableKeyMapping = true;
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
  users.users.${user} = {
    name = user;
    home = "/Users/${user}";
  };

  # services = {
  #   skhd = {
  #     enable = true;
  #     package = pkgs.skhd;
  #     skhdConfig = ''
  #       # Navigation
  #       # Hold Caps Lock and tap 'j' for down arrow
  #       # caps - j : key O
  #       caps - j : key Down

  #       # Hold Caps Lock and tap 'k' for up arrow
  #       # caps - k : key Ö
  #       caps - k : key Up
  #       # # Open Terminal
  #       # alt - return : /Applications/Alacritty.App/Contents/MacOS/alacritty

  #       # # Toggle Window
  #       # lalt - t : yabai -m window --toggle float && yabai -m window --grid 4:4:1:1:2:2
  #       # lalt - f : yabai -m window --toggle zoom-fullscreen
  #       # lalt - q : yabai -m window --close

  #       # # Focus Window
  #       # lalt - up : yabai -m window --focus north
  #       # lalt - down : yabai -m window --focus south
  #       # lalt - left : yabai -m window --focus west
  #       # lalt - right : yabai -m window --focus east

  #       # # Swap Window
  #       # shift + lalt - up : yabai -m window --swap north
  #       # shift + lalt - down : yabai -m window --swap south
  #       # shift + lalt - left : yabai -m window --swap west
  #       # shift + lalt - right : yabai -m window --swap east

  #       # # Resize Window
  #       # shift + cmd - left : yabai -m window --resize left:-50:0 && yabai -m window --resize right:-50:0
  #       # shift + cmd - right : yabai -m window --resize left:50:0 && yabai -m window --resize right:50:0
  #       # shift + cmd - up : yabai -m window --resize up:-50:0 && yabai -m window --resize down:-50:0
  #       # shift + cmd - down : yabai -m window --resize up:-50:0 && yabai -m window --resize down:-50:0

  #       # # Focus Space
  #       # ctrl - 1 : yabai -m space --focus 1
  #       # ctrl - 2 : yabai -m space --focus 2
  #       # ctrl - 3 : yabai -m space --focus 3
  #       # ctrl - 4 : yabai -m space --focus 4
  #       # ctrl - 5 : yabai -m space --focus 5
  #       # #ctrl - left : yabai -m space --focus prev
  #       # #ctrl - right: yabai -m space --focus next

  #       # # Send to Space
  #       # shift + ctrl - 1 : yabai -m window --space 1
  #       # shift + ctrl - 2 : yabai -m window --space 2
  #       # shift + ctrl - 3 : yabai -m window --space 3
  #       # shift + ctrl - 4 : yabai -m window --space 4
  #       # shift + ctrl - 5 : yabai -m window --space 5
  #       # shift + ctrl - left : yabai -m window --space prev && yabai -m space --focus prev
  #       # shift + ctrl - right : yabai -m window --space next && yabai -m space --focus next

  #       # # Menu
  #       # #cmd + space : for now its using the default keybinding to open Spotlight Search
  #     '';
  #   };
  # };

  # system = { keyboard = { enableKeyMapping = true; }; };
}
