{
  pkgs,
  username,
  ...
}:
let
  common = import ./darwin/common.nix;

  extraPackages = with pkgs; [
    fishPlugins.forgit
    fishPlugins.fzf
    glibtool
    kubectx
  ];
in
common {
  inherit pkgs username;
  homeStateVersion = "25.05";
  homeExtraConfig = {
    shell.enableShellIntegration = true;
  };
  inherit extraPackages;
}
