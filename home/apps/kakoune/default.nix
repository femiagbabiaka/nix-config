{ ... }: {
  programs.kakoune = {
    enable = true;
    config = {
      colorScheme = "default";
      numberLines = {
        enable = true;
        highlightCursor = true;
        relative = true;
      };
      ui.assistant = "dilbert";
    };
    defaultEditor = true;
  };
}
