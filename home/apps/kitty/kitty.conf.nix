{ config, ... }:
''
  map super+o remote_control_script ${config.xdg.configHome}/kitty/scripts/broot_with_env

  # Tmux-style splits (ctrl+a prefix)
  # Create splits: - for horizontal, | or \ for vertical (like tmux)
  map ctrl+a>minus launch --location=hsplit --cwd=last_reported
  map ctrl+a>shift+backslash launch --location=vsplit --cwd=last_reported
  map ctrl+a>shift+5 launch --location=hsplit --cwd=last_reported
  map ctrl+a>backslash launch --location=vsplit --cwd=last_reported

  # Create new window (like tmux ctrl+a c)
  map ctrl+a>c new_window_with_cwd

  # Close current pane (like tmux ctrl+a x)
  map ctrl+a>x close_window

  # Switch focus to neighboring window (vim-style hjkl)
  map ctrl+a>h neighboring_window left
  map ctrl+a>l neighboring_window right
  map ctrl+a>k neighboring_window up
  map ctrl+a>j neighboring_window down

  # Arrow key navigation
  map ctrl+left neighboring_window left
  map ctrl+right neighboring_window right
  map ctrl+up neighboring_window up
  map ctrl+down neighboring_window down

  # Resize panes (like tmux ctrl+a + arrow or HJKL)
  map ctrl+a>shift+h resize_window narrower 3
  map ctrl+a>shift+l resize_window wider 3
  map ctrl+a>shift+k resize_window taller 3
  map ctrl+a>shift+j resize_window shorter 3

  # Reset layout sizes
  map ctrl+a>equal resize_window reset

  # Zoom/maximize current pane (like tmux ctrl+a z)
  map ctrl+a>z kitten zoom_toggle.py

  # Rotate windows (like tmux ctrl+a space or ctrl+a o)
  map ctrl+a>space next_layout
  map ctrl+a>o next_window

  # Move window position
  map ctrl+a>shift+braceleft move_window_backward
  map ctrl+a>shift+braceright move_window_forward

  # Tab management (like tmux windows)
  map ctrl+a>t new_tab_with_cwd
  map ctrl+a>n next_tab
  map ctrl+a>p previous_tab
  map ctrl+a>1 goto_tab 1
  map ctrl+a>2 goto_tab 2
  map ctrl+a>3 goto_tab 3
  map ctrl+a>4 goto_tab 4
  map ctrl+a>5 goto_tab 5
  map ctrl+a>6 goto_tab 6
  map ctrl+a>7 goto_tab 7
  map ctrl+a>8 goto_tab 8
  map ctrl+a>9 goto_tab 9

  # Rename tab (like tmux ctrl+a ,)
  map ctrl+a>comma set_tab_title

  # Detach tab to new OS window
  map ctrl+a>d detach_window new-tab

  # Select window by number (like tmux ctrl+a q)
  map ctrl+a>q focus_visible_window

  # Scrollback (like tmux copy mode)
  map ctrl+a>bracketleft show_scrollback
''
