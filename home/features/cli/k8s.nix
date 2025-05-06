{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.cli.k8s;
in {
  options.features.cli.k8s.enable = mkEnableOption "enable k8s cli programs";

  config = mkIf cfg.enable {

    features.impermanence.directories = [ ".kube" ];
    home.packages = with pkgs; [
      fluxcd
      kubectl
      kubectl-cnpg
      argocd
      kubernetes-helm
      kcl
      docker-credential-helpers # needed for kcl packages
      k9s
      kcl-language-server
      kubernetes-helm
      argocd
      kargo
      stable.argocd-autopilot
      minikube
      kind
    ];
    home.sessionVariables = { KIND_EXPERIMENTAL_PROVIDER = "podman"; };
  };
}
