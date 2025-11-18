{ config, ... }:
{
  programs.kitty = {
    enable = true;
    enableGitIntegration = true;
    font = {
      name = "Berkeley Mono";
      size = 16;
    };
    themeFile = "GitHub_Light_High_Contrast";
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
}
