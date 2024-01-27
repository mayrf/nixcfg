{ config, lib, pkgs, user, host, ... }: {

  programs = {
    zathura = {
      enable = true;
      options = { "selection-clipboard" = "clipboard"; };
      # extraConfig = ''
      #   # zathurarc-dark

      #   set font "inconsolata 15"
      #   set default-bg "#000000" #00
      #   set default-fg "#F7F7F6" #01

      #   set statusbar-fg "#B0B0B0" #04
      #   set statusbar-bg "#202020" #01

      #   set inputbar-bg "#151515" #00 currently not used
      #   set inputbar-fg "#FFFFFF" #02

      #   set notification-error-bg "#AC4142" #08
      #   set notification-error-fg "#151515" #00

      #   set notification-warning-bg "#AC4142" #08
      #   set notification-warning-fg "#151515" #00

      #   set highlight-color "#F4BF75" #0A
      #   set highlight-active-color "#6A9FB5" #0D

      #   set completion-highlight-fg "#151515" #02
      #   set completion-highlight-bg "#90A959" #0C

      #   set completion-bg "#303030" #02
      #   set completion-fg "#E0E0E0" #0C

      #   set notification-bg "#90A959" #0B
      #   set notification-fg "#151515" #00

      #   set recolor "true"
      #   set recolor-lightcolor "#000000" #00
      #   set recolor-darkcolor "#E0E0E0" #06
      #   set recolor-reverse-video "true"
      #   set recolor-keephue "true"
      # '';
    };
    wofi = { enable = true; };
    gpg = { enable = true; };
    librewolf.enable = true;
  };

  home.packages = with pkgs; [
    bisq-desktop
    # lutris
    # Cli tools
    ollama
    steam
    img2pdf
    tldr
    rsync # Syncer - $ rsync -r dir1/ dir2/
    unzip # Zip Files
    zip # Zip
    trash-cli
    htop
    git
    git-crypt
    tree
    # TODO add Scripts like:
    # pandoc -f markdown -t org -o ${md%.*}.org ${md};
    pandoc
    sparrow

    # Social
    signal-desktop
    hexchat

    # Dev tools
    docker-compose
    dbeaver
    rnix-lsp
    texlab
    nodePackages_latest.bash-language-server
    emmet-ls

    texlive.combined.scheme-full
    zola
    hugo

    # Media
    deluge
    mpv
    freetube
    tor-browser-bundle-bin
    vlc
    brave
    firefox
    feh # Image Viewer
    sxiv
    urbit
    minitube

    # Productivity
    nextcloud-client
    onlyoffice-bin
    libreoffice
    keepassxc
    calibre
    drawio
    thunderbird
    protonmail-bridge
    armcord
    obsidian

    # Learning
    anki-bin
    tipp10
    exercism
    vimgolf

    #Utils
    gnome.nautilus
    gnome.sushi # A quick previewer for Nautilus
    gnupg
    pinentry-qt # GnuPG’s interface to passphrase input
    ipscan
    qemu
    virt-manager
    offlineimap
    (python3.withPackages (ps: with ps; [ jupyter ]))
    borgbackup
    borgmatic
    sshfs

    # libreoffice-fresh TODO Fix bug relating to "liberation-fonts-ttf-1.07"
    (pkgs.nerdfonts.override { fonts = [ "JetBrainsMono" "iA-Writer" ]; })
  ];
}
