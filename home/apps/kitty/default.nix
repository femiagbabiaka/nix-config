{ config, pkgs, ... }:
{
  # Install kitty kitten scripts to config directory
  xdg.configFile."kitty/zoom_toggle.py" = {
    source = ./scripts/zoom_toggle;
    executable = true;
  };

  # Install shell scripts to PATH
  home.packages = [
    (pkgs.writeShellScriptBin "broot-drawer-toggle" (builtins.readFile ./scripts/broot_drawer_toggle))
    (pkgs.writeShellScriptBin "broot-with-env" ''
      exec ${pkgs.fish}/bin/fish -c 'kitten @ launch --copy-env --cwd=current --type=overlay broot'
    '')
  ];

  programs.kitty = {
    enable = true;
    enableGitIntegration = true;
    font = {
      name = "Berkeley Mono";
      size = 16;
    };
    themeFile = "GitHub_Light_High_Contrast";
    settings = {
      # Enable multiple layouts for split management
      enabled_layouts = "splits,tall:bias=50;full_size=1;mirrored=false,fat,grid,stack";
      allow_remote_control = "yes";
      listen_on = "unix:/tmp/kitty";
      scrollback_lines = 10000;
      enable_audio_bell = false;
      update_check_interval = 0;
      macos_option_as_alt = "both";
      # Visual indicators for active window
      active_border_color = "#00ff00";
      inactive_border_color = "#cccccc";
      window_border_width = "1pt";
    };
    shellIntegration = {
      mode = "enabled";
    };
    extraConfig = import ./kitty.conf.nix { inherit config; };
  };
}
