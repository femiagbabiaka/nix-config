{
  pkgs,
  username,
  dagger,
  ...
}:
let
  myInfra = pkgs.infra.overrideAttrs (
    finalAttrs: previousAttrs: rec {
      version = "0.20.0";
      src = pkgs.fetchFromGitHub {
        owner = "infrahq";
        repo = "infra";
        rev = "v${version}";
        sha256 = "sha256-uz4wimhOfeHSL949m+biIhjfDwwEGnTiJWaz/r3Rsko=";
      };
    }
  );
  myGCSDK = pkgs.google-cloud-sdk.withExtraComponents [
    pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin
    pkgs.google-cloud-sdk.components.kubectl
  ];
in
{
  imports = import ./common-imports.nix ++ [
    ./common-packages.nix
    ./apps/helix
    ./apps/kitty
    ./apps/alacritty
  ];

  home = {
    inherit username;
    stateVersion = "23.05";
  };

  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
  home.packages =
    with pkgs;
    [
      broot
      cloud-provider-kind
      crane
      ctlptl
      delve
      discord
      dive
      docker
      docker-compose
      docker-credential-helpers
      fastly
      fish
      fishPlugins.done
      fishPlugins.forgit
      fishPlugins.hydro
      fluxcd
      git
      git-crypt
      graphviz
      helm-ls
      htop
      kind
      lima
      lldb
      myGCSDK
      myInfra
      neofetch
      nodejs
      presenterm
      spotify
      tilt
      uv
      vault
      wget
      zellij
    ]
    ++ [ dagger.packages.${system}.dagger ];

  fonts.fontconfig.enable = true;
}
