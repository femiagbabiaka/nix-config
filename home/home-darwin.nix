{
  pkgs,
  lib,
  username,
  homeDirectory,
  home-manager,
  config,
  ...
}: let
  myInfra = pkgs.infra.overrideAttrs (finalAttrs: previousAttrs: {
    version = "0.20.0";
  });
  myGCSDK = pkgs.google-cloud-sdk.withExtraComponents [
    pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin
    pkgs.google-cloud-sdk.components.kubectl
  ];
in {
  imports = [
    ./apps/emacs
    ./apps/fish
    ./apps/neovim
    ./apps/git
  ];

  home = {
    inherit username;
    homeDirectory = lib.mkDefault homeDirectory;
    stateVersion = "23.05";
  };

  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
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
    myGCSDK
    gh
    gotools
    gopls
    myInfra
    kubernetes-helm
    jq
    k9s
    kubectx
    nil
    nushell
    platinum-searcher
    rbenv
    ripgrep
    roswell
    rustup
    shellcheck
    spotify
    stern
    terraform-docs
    tflint
    terraform-ls
    tfswitch
    tmux
    tmux-xpanes
    zstd
    zoxide
    (pkgs.nerdfonts.override {fonts = ["FiraCode"];})
  ];

  fonts.fontconfig.enable = true;
}
