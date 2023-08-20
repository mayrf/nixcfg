{ config, lib, pkgs, user, host, ... }:
{

  programs = {
    zathura = {
      enable = true;
      options = {
        "selection-clipboard" = "clipboard";
      };
    };
    wofi = { enable = true; };
    foot = {
      enable = true;
      server.enable = true;
      settings = {
        main = {
          term = "xterm-256color";

          font = "JetBrainsMono Nerd Font:size=10";
          dpi-aware = "yes";
        };
        mouse = {
          hide-when-typing = "yes";
        };
      };
    };
    gpg = { enable = true; };
    git = {
      enable = true;
      userName = "mayrf";
      userEmail = "70516376+mayrf@users.noreply.github.com";
      # config = {
      #   commit = {
      #     template = ./commitMsg.txt;
      #   };
      # };

    };
    librewolf.enable = true;
  };
  home.packages = with pkgs; [
    # Testing
    # TODO packages not working when only available projectwise
    # nodePackages.eslint
    nodePackages.prettier
    # nodePackages.typescript-language-server**

    # Cli tools
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

    # Social
    signal-desktop
    schildichat-desktop

    # Dev tools
    docker-compose
    awscli2
    dbeaver
    hugo
    rnix-lsp
    texlive.combined.scheme-full

    # Media
    deluge
    mpv
    freetube
    tor-browser-bundle-bin
    vlc
    brave
    feh # Image Viewer
    sxiv
    urbit

    # Productivity
    nextcloud-client
    onlyoffice-bin
    keepassxc
    calibre

    # Learning
    anki-bin
    tipp10
    exercism
    vimgolf

    #Utils
    gnome.nautilus
    gnome.sushi # A quick previewer for Nautilus
    gnupg
    pinentry_qt # GnuPG’s interface to passphrase input
    ipscan
    riseup-vpn
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
