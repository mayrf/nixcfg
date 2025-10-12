{ config, pkgs, ... }:
let
  immichVersion = "v2.0.1";

  immichRoot =
    "/var/lib/immich"; # TODO: Tweak these to your desired storage locations
  immichAppdataRoot = "${immichRoot}/appdata";
in {
  features.impermanence.directories_cache = [ "/var/lib/immich" ];

  systemd.tmpfiles.rules = [
    "d /var/lib/immich 0755 root root -"
    "d /var/lib/immich/appdata 0755 root root -"
    "d /var/lib/immich/appdata/model-cache 0755 root root -"
  ];
  features.docker.enable = true;
  virtualisation.oci-containers = {
    containers = {

      immich-machine-learning = {
        image =
          "ghcr.io/immich-app/immich-machine-learning:${immichVersion}-rocm";

        ports = [ "3003:3003" ];
        environment = {
          IMMICH_VERSION = immichVersion;
          HSA_OVERRIDE_GFX_VERSION = "10.3.0";
        };
        volumes = [ "${immichAppdataRoot}/model-cache:/cache" ];
        extraOptions = [
          "--group-add=video"
          "--device=/dev/dri:/dev/dri"
          "--device=/dev/kfd:/dev/kfd"
        ];
      };
    };
  };
  networking.firewall = { allowedTCPPorts = [ 3003 ]; };
}
