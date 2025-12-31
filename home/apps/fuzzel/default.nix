# Fuzzel application launcher configuration
{ config, pkgs, lib, ... }:

{
  programs.fuzzel = {
    enable = true;

    settings = {
      main = {
        terminal = "kitty";
        layer = "overlay";
        width = 50;
        font = "JetBrains Mono:size=12";
        prompt = "> ";
        icons-enabled = true;
        icon-theme = "Adwaita";
        lines = 12;
        horizontal-pad = 20;
        vertical-pad = 10;
        inner-pad = 5;
      };

      colors = {
        # Nord-inspired dark theme
        background = "2e3440ee";
        text = "d8dee9ff";
        match = "88c0d0ff";
        selection = "4c566aff";
        selection-text = "eceff4ff";
        selection-match = "8fbcbbff";
        border = "88c0d0ff";
      };

      border = {
        width = 2;
        radius = 8;
      };

      dmenu = {
        exit-immediately-if-empty = false;
      };

      key-bindings = {
        cancel = "Escape Control+c";
        execute = "Return KP_Enter";
        cursor-left = "Left Control+b";
        cursor-right = "Right Control+f";
        delete-prev = "BackSpace";
        delete-next = "Delete";
        delete-prev-word = "Control+BackSpace Control+w";
        delete-next-word = "Control+Delete";
        prev = "Up Control+p";
        next = "Down Control+n";
      };
    };
  };
}
