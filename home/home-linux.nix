{
  pkgs,
  lib,
  username,
  homeDirectory,
  home-manager,
  config,
  ...
}: {
  imports = [
    ./apps/emacs
    ./apps/fish
    ./apps/neovim
    ./apps/git
    ./apps/ssh
  ];

  home = {
    inherit username;
    homeDirectory = lib.mkDefault homeDirectory;
    stateVersion = "23.05";
  };

  programs.home-manager.enable = true;
  programs.fish.enable = true;
  nixpkgs.config.allowUnfree = true;

  programs.firefox = {enable = true;};
  programs.nix-index = {
    enable = true;
    enableFishIntegration = true;
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
    guardian-agent
    haskell-language-server
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
    rbenv
    ripgrep
    roswell
    rustup
    shellcheck
    spotify
    stern
    tailscale
    _1password-gui
    firefox
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
    (pkgs.nerdfonts.override {fonts = ["FiraCode"];})
    nil
  ];

  fonts.fontconfig.enable = true;
}
