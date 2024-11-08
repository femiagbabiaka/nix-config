{
  ...
}:
{
  programs.helix = {
    enable = true;
    defaultEditor = true;
    ignores = [
      ".terraform/*"
      ".git/*"
    ];
    settings = {
      theme = "github_dark";
      editor = {
        lsp.display-messages = true;
      };
    };
  };
}
