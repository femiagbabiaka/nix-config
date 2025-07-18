{
  pkgs,
  username,
  ...
}:
{
  imports = import ./common-imports.nix ++ [
    ./common-packages.nix
    ./apps/kitty
  ];

  home = {
    inherit username;
    stateVersion = "25.05";
    shell.enableShellIntegration = true;
  };

  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    broot
    lima
    claude-code
    crane
    docker
    docker-compose
    delve
    dive
    git-crypt
    graphviz
    helm-ls
    lldb
    nodejs
    roswell
    tilt
    git
    neofetch
    htop
    fish
    wget
    fishPlugins.done
    fishPlugins.forgit
    fishPlugins.hydro
    fishPlugins.fzf
    glibtool
  ]; # ++ [ linux-pkgs.kak-tree-sitter ];

  fonts.fontconfig.enable = true;
}
