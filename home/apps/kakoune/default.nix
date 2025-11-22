{ pkgs, ... }:
{
  programs.kakoune = {
    enable = true;
    config = {
      colorScheme = "solarized-dark";
      numberLines = {
        enable = true;
        highlightCursor = true;
      };
      ui.assistant = "dilbert";
    };
    defaultEditor = true;
    plugins = with pkgs.kakounePlugins; [
      auto-pairs-kak
      smarttab-kak
    ];
  };
}
