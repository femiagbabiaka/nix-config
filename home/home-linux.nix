{ pkgs, lib, username, homeDirectory, home-manager, config, ... }:
{
  imports = [
    ./apps/emacs
    ./apps/fish
    ./apps/neovim
    ./apps/git
    ./apps/i3
  ];

  home = {
    inherit username;
    homeDirectory = lib.mkDefault homeDirectory;
    stateVersion = "23.05";
  };

  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;

  programs.nushell.enable = true;
  programs.firefox = { enable = true; };
}
