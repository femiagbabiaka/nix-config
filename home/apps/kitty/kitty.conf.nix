{ config, ... }:
''
  map --new-mode mw --on-action end ctrl+w
  map --mode mw left neighboring_window left
  map --mode mw right neighboring_window right
  map --mode mw up neighboring_window up
  map --mode mw down neighboring_window down

  map --mode mw h neighboring_window left
  map --mode mw l neighboring_window right
  map --mode mw k neighboring_window up
  map --mode mw j neighboring_window down
  map --mode mw z toggle_layout stack

  map super+o remote_control_script ${config.xdg.configHome}/kitty/scripts/broot_with_env
''
