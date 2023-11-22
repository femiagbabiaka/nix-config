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
    haskell-language-server
    kubernetes-helm
    jq
    k9s
    kubectx
    mitschemeX11
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
    (pkgs.nerdfonts.override {fonts = ["FiraCode"];})
    nil
  ];

  fonts.fontconfig.enable = true;
}
