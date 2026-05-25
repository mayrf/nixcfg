{ ... }:
{
  flake.modules.homeManager.productivity =
    { config, pkgs, ... }:
    let
      gnucash-de = pkgs.symlinkJoin {
        name = "gnucash-de";
        paths = [ pkgs.gnucash ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          # Wrap the binary
          wrapProgram $out/bin/gnucash \
            --set LANGUAGE "de_DE" \
            --set LANG "de_DE.UTF-8"

          # Replace symlinked .desktop with a patched copy
          rm $out/share/applications/gnucash.desktop
          cp ${pkgs.gnucash}/share/applications/gnucash.desktop \
             $out/share/applications/gnucash.desktop
          substituteInPlace $out/share/applications/gnucash.desktop \
            --replace-fail \
              "Exec=gnucash" \
              "Exec=env LANGUAGE=de_DE LANG=de_DE.UTF-8 gnucash"
        '';
      };
    in
    {
      features.impermanence.directories = [
        ".config/calibre"
        ".thunderbird"
        ".local/share/gnucash"
        ".config/keepassxc"
        ".cache/keepassxc"
        ".config/libreoffice"
        ".wine"
        ".config/immich"
      ];

      home.packages = with pkgs; [
        stable.calibre
        drawio
        thunderbird
        obsidian
        inkscape
        reaper
        stable.scribus
        gimp
        # stable.gnucash
        gnucash-de
        mmex
        stable.ardour
        onlyoffice-desktopeditors
        temurin-jre-bin-21
        libreoffice-qt6-fresh
        keepassxc
        legcord
        stable.ipscan
        dbgate
        wine
        immich-cli
      ];
    };
}
