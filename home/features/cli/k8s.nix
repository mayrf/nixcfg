{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.cli.k8s;
in {
  options.features.cli.k8s.enable = mkEnableOption "enable k8s cli programs";

  config = mkIf cfg.enable {

    features.impermanence.files = [ ".kube/config" ];
    home.packages = with pkgs; [
      fluxcd
      kind
      kubectl
      kubectl-cnpg
      argocd
      kubernetes-helm
      kcl
      k9s
      kcl-language-server
      kubernetes-helm
      argocd
      kargo
      stable.argocd-autopilot
    ];
  };
}
