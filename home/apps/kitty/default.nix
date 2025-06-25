{ config, ... }: {
  programs.kitty = {
    enable = true;
    enableGitIntegration = true;
    font = {
      name = "FiraCode Nerd Font Mono";
      size = 16;
    };
    keybindings = {
      "ctrl+a>left" = "neighboring_window left";
      "ctrl+a>right" = "neighboring_window right";
      "ctrl+a>up" = "neighboring_window up";
      "ctrl+a>down" = "neighboring_window down";
      "ctrl+a>l" = "neighboring_window left";
      "ctrl+a>h" = "neighboring_window right";
      "ctrl+a>k" = "neighboring_window up";
      "ctrl+a>j" = "neighboring_window down";
    };
    themeFile = "ayu";
    settings = {
      enabled_layouts = "splits:split_axis=horizontal";
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

