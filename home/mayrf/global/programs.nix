{ pkgs, pkgs-stable, ... }: {

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

    gnucash
    oterm
    tmux
    nerdctl
    fzf
    file
    nodejs
    kubectl
    argocd
    kubernetes-helm
    sxiv

    bisq-desktop
    # lutris
    # Cli tools
    gparted
    # steam
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
    statix

    # Social
    signal-desktop
    hexchat
    logseq

    # Dev tools
    dbeaver-bin
    texlab

    texlive.combined.scheme-full
    zola
    hugo

    # Media
    deluge
    mpv
    freetube
    tor-browser
    vlc
    brave
    firefox
    feh # Image Viewer
    sxiv
    urbit
    minitube
    ardour

    # Productivity
    nextcloud-client
    onlyoffice-bin
    temurin-jre-bin-21
    pkgs-stable.libreoffice-qt6-fresh
    keepassxc
    pkgs-stable.calibre
    drawio
    thunderbird
    protonmail-bridge
    armcord
    obsidian
    krita
    inkscape
    reaper
    ardour

    # Learning
    anki-bin
    tipp10
    exercism
    vimgolf

    #Utils
    just
    age
    nautilus
    sushi # A quick previewer for Nautilus
    gnupg
    pinentry-qt # GnuPG s interface to passphrase input
    ipscan
    offlineimap
    (python3.withPackages (ps: with ps; [ jupyter ]))
    borgbackup
    borgmatic
    sshfs
    jq
    ijq

    pkgs-stable.kcl-cli

    # libreoffice-fresh TODO Fix bug relating to "liberation-fonts-ttf-1.07"
    (pkgs.nerdfonts.override { fonts = [ "JetBrainsMono" "iA-Writer" ]; })
  ];
}
