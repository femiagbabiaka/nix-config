{ pkgs, lib, username, homeDirectory, home-manager, config, ... }: {
  home = {
    inherit username;
    homeDirectory = lib.mkDefault homeDirectory;
    stateVersion = "23.05";
  };
  programs.nushell.enable = true;
  programs.firefox = { enable = true; };
  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
    extraConfig = ''
      (org-babel-load-file "${homeDirectory}/.emacs.d/configuration.org")
    '';
  };

  home.file.".emacs.d/early-init.el".text = ''
    (setq package-enable-at-startup nil)
  '';
  home.file.".emacs.d/configuration.org".source =
    config.lib.file.mkOutOfStoreSymlink ./emacs/configuration.org;
  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
  programs.fish = {
    enable = true;
    interactiveShellInit = builtins.readFile ./fish/init.fish;
    shellAliases = {
      vim = "emacsclient -c";
      gc = "git commit";
      gp = "git push";
    };
  };

  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = "Mod1";
      fonts = {
        names = [ "pango:DejaVu Sans Mono" ];
        size = 8.0;
      };
      keybindings = lib.mkOptionDefault {
        "XF86AudioRaiseVolume" =
          "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +10%";
        "XF86AudioLowerVolume" =
          "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -10%";
        "XF86AudioMute" =
          "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
        "XF86AudioMicMute" =
          "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle";
        "XF86MonBrightnessUp" = "exec ${pkgs.light}/bin/light -A 10";
        "XF86MonBrightnessDown" = "exec ${pkgs.light}/bin/light -U 10";
      };
    };
  };

  programs.neovim = {
    enable = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [ vim-nix vim-sensible ];
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
  programs.git = {
    enable = true;
    userName = "Femi Agbabiaka";
    userEmail = "femi@femiagbabiaka.xyz";
    delta = { enable = true; };
  };
}
