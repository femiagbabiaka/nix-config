{
  pkgs,
  lib,
  username,
  homeDirectory,
  ...
}:
{
  imports = import ./common-imports.nix ++ [
    ./common-packages.nix
    ./apps/ssh
    ./apps/kakoune
  ];

  home = {
    inherit username;
    homeDirectory = lib.mkDefault homeDirectory;
    stateVersion = "23.05";
  };

  programs.home-manager.enable = true;
  programs.fish.enable = true;
  nixpkgs.config.allowUnfree = true;

  programs.firefox = {
    enable = true;
  };

  home.packages = with pkgs; [
    fastly
    google-cloud-sdk
    gotools
    ghc
    ghostty
    haskell-language-server
    mpv
    stack
    mitschemeX11
    mosh
    pulseaudio
    roswell
    spotify
    tailscale
    _1password-gui
    yubioath-flutter
    tmux
    tmux-xpanes
    zathura
    zellij
  ];

  fonts.fontconfig.enable = true;
}
