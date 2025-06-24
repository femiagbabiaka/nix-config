{ pkgs, lib, username, homeDirectory, home-manager, config, ... }: {
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ LazyVim ];
  };
}
