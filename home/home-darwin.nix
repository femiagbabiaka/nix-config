{ pkgs, lib, username, homeDirectory, home-manager, config, ... }:
{
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
    google-cloud-sdk
    gh
    gotools
    gopls
    infra
    jq
    k9s
    kubectx
    nushell
    platinum-searcher
    rbenv
    ripgrep
    rustup
    shellcheck
    terraform-docs
    tflint
    terraform-ls
    tfswitch
    tmux
    tmux-xpanes
    zstd
    zoxide
    (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];

  fonts.fontconfig.enable = true;
}
