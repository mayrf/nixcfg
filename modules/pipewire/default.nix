{ ... }:
{
  flake.modules.nixos.pipewire =
    { ... }:
    {
      security.rtkit.enable = true;
      services.pulseaudio.enable = false;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        jack.enable = true;
      };

      # Better Bluetooth audio via WirePlumber
      services.pipewire.wireplumber.extraConfig."10-bluez" = {
        "monitor.bluez.properties" = {
          "bluez5.enable-sbc-xq" = true; # Better audio quality (A2DP)
          "bluez5.enable-msbc" = true; # Better mic quality (HFP) — 16kHz vs 8kHz
          "bluez5.enable-hw-volume" = true;
          "bluez5.roles" = [
            "hsp_hs"
            "hsp_ag"
            "hfp_hf"
            "hfp_ag"
          ];
        };
      };
    };
}
