{
  pkgs,
  lib,
  username,
  homeDirectory,
  home-manager,
  config,
  ...
}: {
  programs.gh = {
    enable = true;
    gitCredentialHelper = {
      enable = true;
    };
  };

  programs.git = {
    enable = true;
    userName = "Femi Agbabiaka";
    userEmail = "femi@femiagbabiaka.xyz";
    delta = {enable = true;};
    extraConfig = {
      core = {
        editor = "hx";
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
      url = {
        "git@github.com:fastly" = {
          insteadof = "https://github.com/fastly";
        };
      };
    };
  };
}
