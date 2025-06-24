{ config, ... }: {
  programs.kitty = {
    enable = true;
    font = {
      name = "FiraCode Nerd Font Mono";
      size = 16;
    };
    themeFile = "ayu";
    settings = {
      allow_remote_control = "yes";
      scrollback_lines = 10000;
      enable_audio_bell = false;
      update_check_interval = 0;
      macos_option_as_alt = "both";
    };
    shellIntegration = { mode = "enabled"; };
  };
  home.file."${config.xdg.configHome}/kitty/open-actions.conf" = {
    source = ./open-actions.conf;
  };
}

