{ ... }:
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
    extraConfig = builtins.readFile ./extraconfig.kak;
  };
}
