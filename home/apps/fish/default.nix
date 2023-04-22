{ pkgs, lib, username, homeDirectory, home-manager, config, ... }:
{
  programs.fish = {
    enable = true;
    interactiveShellInit = builtins.readFile ./init.fish;
    shellAliases = {
      vim = "emacsclient -c";
      gc = "git commit";
      gp = "git push";
    };
    plugins = [
      {
        name = "nix-env.fish";
        src = pkgs.fetchFromGitHub {
          owner = "lilyball";
          repo = "nix-env.fish";
          rev = "7b65bd228429e852c8fdfa07601159130a818cfa";
          sha256 = "RG/0rfhgq6aEKNZ0XwIqOaZ6K5S4+/Y5EEMnIdtfPhk=";
        };
      }
    ];
  };
}
