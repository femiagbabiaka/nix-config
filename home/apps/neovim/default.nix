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
  programs.neovim = {
    enable = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-sensible
    ];
    extraConfig = ''
      set number
      set cc=80
      set list
      set listchars=tab:→\ ,space:·,nbsp:␣,trail:•,eol:¶,precedes:«,extends:»
      if &diff
        colorscheme blue
      endif
    '';
  };
}
