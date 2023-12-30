{ config, lib, pkgs, user, host, ... }: {

  programs = {
    zathura = {
      enable = true;
      options = { "selection-clipboard" = "clipboard"; };
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

    texlive.combined.scheme-full

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
