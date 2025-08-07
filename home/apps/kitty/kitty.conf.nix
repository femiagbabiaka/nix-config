{ config, ... }:
''
  map super+o remote_control_script ${config.xdg.configHome}/kitty/scripts/broot_with_env
  map ctrl+a>minus launch --location=hsplit --cwd=last_reported
  map ctrl+a>shift+backslash launch --location=vsplit
  # Switch focus to the neighboring window in the indicated direction
  map ctrl+left neighboring_window left
  map ctrl+right neighboring_window right
  map ctrl+up neighboring_window up
  map ctrl+down neighboring_window down
  map ctrl+a>h neighboring_window left
  map ctrl+a>l neighboring_window right
  map ctrl+a>k neighboring_window up
  map ctrl+a>j neighboring_window down
  map ctrl+a>z remote_control_script ${config.xdg.configHome}/kitty/scripts/zoom_toggle
''
