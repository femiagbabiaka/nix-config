{ ... }: {
  programs.helix = {
    enable = true;
    ignores = [ ".terraform/*" ".git/*" ];
    settings = {
      theme = "github_dark";
      editor = {
        lsp.display-messages = true;
        cursor-shape.insert = "bar";
      };
    };
  };
}
