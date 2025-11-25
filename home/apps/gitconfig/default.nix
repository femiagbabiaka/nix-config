{
  pkgs,
  lib,
  username,
  homeDirectory,
  home-manager,
  config,
  ...
}:
{
  programs.gh = {
    enable = true;
    gitCredentialHelper = {
      enable = true;
    };
  };

  programs.git = {
    enable = true;
    settings = {
      user = {
        name = "Femi Agbabiaka";
        email = "femi@femiagbabiaka.xyz";
      };
      core = {
        editor = "hx";
        excludesFile = "~/.gitignore";
      };
      github = {
        user = "femiagbabiaka";
      };
      push = {
        autoSetupRemote = true;
      };
      pull = {
        rebase = true;
      };
      url = {
        "ssh://git@github.com/" = {
          insteadof = "https://github.com/";
        };
      };
    };
  };

  programs.delta = {
    enable = true;
    enableGitIntegration = true;
  };
}
