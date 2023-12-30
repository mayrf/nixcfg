{ pkgs }:

let
  # https://unsplash.com/photos/tV06QVJXVxU
  imgLink = "https://images.unsplash.com/photo-1486570318579-054c95b01160";

  image = pkgs.fetchurl {
    url = imgLink;
    sha256 = "sha256-6A34obGpDh9vHwodRVZYOwsCo01XRv9ZZPUcy7zHe58=";
  };
in pkgs.stdenv.mkDerivation {
  name = "sddm-theme";
  src = pkgs.fetchFromGitHub {
    owner = "MarianArlt";
    repo = "sddm-sugar-dark";
    rev = "ceb2c455663429be03ba62d9f898c571650ef7fe";
    sha256 = "0153z1kylbhc9d12nxy9vpn0spxgrhgy36wy37pk6ysq7akaqlvy";
  };
  installPhase = ''
    mkdir -p $out
    cp -R ./* $out/
    cd $out/
    rm Background.jpg
    echo "ForceHideCompletePassword=true" >> $out/theme.conf
    cp -r ${image} $out/Background.jpg
  '';
}
