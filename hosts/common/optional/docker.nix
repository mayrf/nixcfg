{ pkgs, ... }: {
  virtualisation.docker.enable = true;
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };
  virtualisation.containerd.enable = true;

  environment.systemPackages = with pkgs; [
    docker-compose
    docker-credential-helpers
  ];

}
