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
  programs.fish = {
    enable = true;
    interactiveShellInit = builtins.readFile ./init.fish;
    shellAliases = {
      vim = "et";
      gc = "git commit";
      gp = "git push";
      cat = "bat";
      subl = "/Applications/Sublime'\ 'Text.app/Contents/SharedSupport/bin/subl";
      mosdef_generate_app = "chef generate cookbook -g ~/generators/fst_generator_app_cookbook -C 'Fastly, Inc.' -m 'team-sre@fastly.com'";
      mosdef_generate_lib = "chef generate cookbook -g ~/generators/fst_generator_library_cookbook -C 'Fastly, Inc.' -m 'team-sre@fastly.com'";
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
      {
        name = "emacs.fish";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-emacs";
          rev = "af69fd49352137118f2e7b8603759b823c342041";
          sha256 = "6qRDyVdDjulbjlP5S+k6Z9Yh9c5tODQY7EdcNDnSlcA=";
        };
      }
      {
        name = "tmux.fish";
        src = pkgs.fetchFromGitHub {
          owner = "budimanjojo";
          repo = "tmux.fish";
          rev = "db0030b7f4f78af4053dc5c032c7512406961ea5";
          sha256 = "rRibn+FN8VNTSC1HmV05DXEa6+3uOHNx03tprkcjjs8=";
        };
      }
    ];
  };
}
