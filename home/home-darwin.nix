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
    cmake
    colima
    coreutils
    curl
    fastly
    fd
    fq
    gh
    gopls
    infra
    jq
    k9s
    kubectx
    platinum-searcher
    rbenv
    shellcheck
    terraform-docs
    tflint
    terraform-ls
    tfswitch
    tmux
    tmux-xpanes
    zoxide
  ];

  programs.nushell.enable = true;
}
