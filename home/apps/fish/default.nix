{
  pkgs,
  ...
}:
{
  programs.fish = {
    enable = true;
    interactiveShellInit = builtins.readFile ./init.fish;
    shellAliases = {
      e = "emacs -nw";
      vim = "nvim";
      gc = "git commit";
      gp = "git push";
      cat = "bat";
      mosdef_generate_app = "chef generate cookbook -g ~/generators/fst_generator_app_cookbook -C 'Fastly, Inc.' -m 'team-sre@fastly.com'";
      mosdef_generate_lib = "chef generate cookbook -g ~/generators/fst_generator_library_cookbook -C 'Fastly, Inc.' -m 'team-sre@fastly.com'";
      kubectx = "${pkgs.kubeswitch}/bin/switcher";
    };
    plugins = [
      {
        name = "fishbang";
        src = pkgs.fetchFromGitHub {
          owner = "BrewingWeasel";
          repo = "fishbang";
          rev = "6382cbdd171cae8385f07accac5b87984fb153d6";
          sha256 = "sha256-AJwaJ3Khu25EWEkJdv7lemO0T0gjcx9xtN6MnxQ9v6I=";
        };
      }
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
        name = "fish-ssh-agent";
        src = pkgs.fetchFromGitHub {
          owner = "danhper";
          repo = "fish-ssh-agent";
          rev = "f10d95775352931796fd17f54e6bf2f910163d1b";
          sha256 = "sha256-cFroQ7PSBZ5BhXzZEKTKHnEAuEu8W9rFrGZAb8vTgIE=";
        };
      }
    ];
  };
}
