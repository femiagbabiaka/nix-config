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
    matchBlocks = {
      "*" = {
        controlMaster = "auto";
        controlPath = "~/.ssh/master-%r@%h:%p";
        controlPersist = "1h";
      };
      "codeberg.org" = {
        identityFile = "~/.ssh/id_yubikey";
        identityAgent = "none";
      };
      "brain-2" = {
        identityFile = "~/.ssh/id_yubikey";
        identityAgent = "none";
      };
      "github.com" = {
        identityFile = "~/.ssh/id_yubikey";
        identityAgent = "none";
      };
      "cassiopeia" = {
        identityFile = "~/.ssh/id_yubikey";
        identityAgent = "none";
      };
      "tachibana" = {
        identityFile = "~/.ssh/id_yubikey";
        identityAgent = "none";
      };
    };
  };
}
