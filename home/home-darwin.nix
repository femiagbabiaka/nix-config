{
  pkgs,
  username,
  dagger,
  system,
  ...
}:
let
  common = import ./darwin/common.nix;

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

  extraPackages =
    (with pkgs; [
      cloud-provider-kind
      claude-code
      codex
      ctlptl
      discord
      docker-credential-helpers
      fastly
      fluxcd
      git-lfs
      kind
      kubeswitch
      presenterm
      uv
      vault
      zellij
    ])
    ++ [
      myGCSDK
      myInfra
      dagger.packages.${system}.dagger
    ];
in
common {
  inherit pkgs username;
  extraImports = [
    ./apps/helix
    ./apps/alacritty
  ];
  inherit extraPackages;
}
