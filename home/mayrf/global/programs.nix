{ pkgs, pkgs-stable, outputs, ... }: {

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
    pkgs-stable.oterm
    nerdctl
    tmux
    file
    nodejs
    # TODO Make kubectl plugin installs declarative (example below: cnpg from "https://cloudnative-pg.io/documentation/1.24/kubectl-plugin/")
    # curl -sSfL \
    # https://github.com/cloudnative-pg/cloudnative-pg/raw/main/hack/install-cnpg-plugin.sh | \
    # sudo sh -s -- -b /usr/local/bin
    argocd
    kubernetes-helm
    sxiv
    devbox
    outputs.packages.x86_64-linux.kcl-language-server

    # lutris
    # Cli tools
    gparted
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
    sops
    sxiv
    devbox
    fzf
    # TODO add Scripts like:
    # pandoc -f markdown -t org -o ${md%.*}.org ${md};
    pandoc
    statix

    # Social
    signal-desktop
    hexchat
    # logseq

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
    # pkgs-stable.calibre
    calibre
    drawio
    thunderbird
    protonmail-bridge
    legcord
    obsidian
    krita
    inkscape
    reaper
    ardour
    scribus
    gimp
    gnucash
    # pkgs-stable.bisq-desktop
    # bisq-desktop
    bisq2
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
    pkgs-stable.ipscan
    offlineimap
    (python3.withPackages (ps: with ps; [ jupyter ]))
    borgbackup
    borgmatic
    sshfs
    jq
    ijq
    compose2nix

    # kubernetes related
    fluxcd
    kubectl
    argocd
    kubernetes-helm
    kcl
    k9s
    outputs.packages.x86_64-linux.kcl-language-server
    outputs.packages.x86_64-linux.httpyac

    nfs-utils
    # pkgs-stable.kcl-cli

    # libreoffice-fresh TODO Fix bug relating to "liberation-fonts-ttf-1.07"
    nerd-fonts.im-writing
    nerd-fonts.jetbrains-mono
    nerd-fonts.fira-code
  ];
}
