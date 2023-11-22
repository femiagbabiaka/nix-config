{
  pkgs,
  lib,
  username,
  homeDirectory,
  home-manager,
  config,
  ...
}: {
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "cassiopeia" = {
        forwardAgent = true;
      };
    };
  };
}
