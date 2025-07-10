{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.cli.k8s;
in {
  options.features.cli.k8s.enable = mkEnableOption "enable k8s cli programs";

  config = mkIf cfg.enable {

    features.impermanence.directories = [ ".kube" ];
    home.packages = with pkgs; [
      auger
      fluxcd
      kubectl
      kubectl-cnpg
      kubectl-tree 
      argocd
      kubernetes-helm
      unstable.kcl
      crossplane-cli
      docker-credential-helpers # needed for kcl packages
      k9s
      argocd-lovely-plugin
      unstable.kcl-language-server
      kubernetes-helm
      argocd
      kargo
      stable.argocd-autopilot
      minikube
      kind
      kustomize
    ];
    home.sessionVariables = { KIND_EXPERIMENTAL_PROVIDER = "podman"; };
  };
}
