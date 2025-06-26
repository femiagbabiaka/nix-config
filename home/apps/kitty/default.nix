{ config, ... }:
{
  programs.kitty = {
    enable = true;
    enableGitIntegration = true;
    extraConfig = import ./kitty.conf.nix { inherit config; };
    font = {
      name = "FiraCode Nerd Font Mono";
      size = 16;
    };
    themeFile = "GitHub_Dark_High_Contrast";
    settings = {
      enabled_layouts = "splits:split_axis=horizontal,stack";
      allow_remote_control = "yes";
      scrollback_lines = 10000;
      enable_audio_bell = false;
      update_check_interval = 0;
      macos_option_as_alt = "both";
    };
    shellIntegration = {
      mode = "enabled";
    };
  };
  home.file."${config.xdg.configHome}/kitty/open-actions.conf" = {
    source = ./open-actions.conf;
  };
  home.file."${config.xdg.configHome}/kitty/scripts" = {
    source = ./scripts;
    recursive = true;
    executable = true;
  };
}
