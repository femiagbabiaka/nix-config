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
  imports = [
    ./apps/fish
    ./apps/neovim
    ./apps/git
    ./apps/ssh
    ./apps/helix
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
    ansible
    automake
    awscli2
    bat
    cmake
    colima
    coreutils
    curl
    fastly
    fd
    fq
    go
    google-cloud-sdk
    gh
    gotools
    gopls
    ghc
    ghostty
    haskell-language-server
    helix
    mpv
    stack
    kubernetes-helm
    jq
    k9s
    kubectx
    mitschemeX11
    mosh
    nushell
    platinum-searcher
    pulseaudio
    rbenv
    ripgrep
    roswell
    rustup
    shellcheck
    spotify
    stern
    tailscale
    _1password-gui
    yubioath-flutter
    terraform-docs
    tflint
    terraform-ls
    tfswitch
    tmux
    tmux-xpanes
    zstd
    zoxide
    zathura
    zellij
    nerd-fonts.fira-code
    nil
  ];

  fonts.fontconfig.enable = true;
}
