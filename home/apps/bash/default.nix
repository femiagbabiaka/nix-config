{
  ...
}:
{
  programs.bash = {
    enable = true;
    enableCompletion = true;
    profileExtra = ''
      # Prepend paths (highest priority)
      export PATH="$HOME/.config/emacs/bin:$HOME/.rd/bin:$PATH"
      # Append paths (Go paths)
      export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin"
    '';
  };
}
