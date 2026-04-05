{
  pkgs,
  lib,
  username,
  homeDirectory,
  home-manager,
  config,
  ...
}:
{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "*" = {
        controlMaster = "auto";
        controlPath = "~/.ssh/master-%r@%h:%p";
        controlPersist = "1h";
      };
    };
  };
}
