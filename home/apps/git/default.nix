{ pkgs, lib, username, homeDirectory, home-manager, config, ... }:
{
  programs.gh = {
    enable = true;
    enableGitCredentialHelper = true;
  };

  programs.git = {
    enable = true;
    userName = "Femi Agbabiaka";
    userEmail = "femi@femiagbabiaka.xyz";
    delta = { enable = true; };
    extraConfig = {
      core = {
        editor = "emacs -nw";
        excludesFile = "~/.gitignore";
      };
      github = {
        user = "femiagbabiaka";
      };
      push = {
        autoSetupRemote = "true";
      };
      pull = {
        rebase = "true";
      };
    };
  };
}
