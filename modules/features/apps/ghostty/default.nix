{ ... }:
{
  flake.modules.homeManager.ghostty =
    { ... }:
    {
      programs.ghostty = {
        enable = true;
        enableZshIntegration = true;
        installVimSyntax = true;
        settings = {
          gtk-titlebar = false;
          shell-integration = "zsh";
        };
      };
    };
}
