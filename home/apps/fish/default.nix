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
      subl = "/Applications/Sublime' 'Text.app/Contents/SharedSupport/bin/subl";
      mosdef_generate_app = "chef generate cookbook -g ~/generators/fst_generator_app_cookbook -C 'Fastly, Inc.' -m 'team-sre@fastly.com'";
      mosdef_generate_lib = "chef generate cookbook -g ~/generators/fst_generator_library_cookbook -C 'Fastly, Inc.' -m 'team-sre@fastly.com'";
      kubectx = "${pkgs.kubeswitch}/bin/switcher";
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
