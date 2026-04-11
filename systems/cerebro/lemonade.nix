{ config, lib, pkgs, ... }:

let
  # Override llama-cpp to b8681 for gemma4 architecture support (added in b8637)
  llama-cpp-b8681 = backend: (pkgs.llama-cpp.override backend).overrideAttrs (old: {
    version = "8681";
    src = pkgs.fetchFromGitHub {
      owner = "ggml-org";
      repo = "llama.cpp";
      tag = "b8681";
      hash = "sha256-axkI+Argy3vtjI6QqQbYncTzaJno2TOsKgLHzGRCwbc=";
      leaveDotGit = true;
      postFetch = ''
        git -C "$out" rev-parse --short HEAD > $out/COMMIT
        find "$out" -name .git -print0 | xargs -0 rm -rf
      '';
    };
    postPatch = ""; # index.html.gz no longer exists in b8681
  });
  llama-cpp-rocm-gemma4 = llama-cpp-b8681 { rocmSupport = true; };
  llama-cpp-vulkan-gemma4 = llama-cpp-b8681 { vulkanSupport = true; };

  version = "10.0.1";

  lemonade-server = pkgs.stdenv.mkDerivation {
    pname = "lemonade-server";
    inherit version;

    src = pkgs.fetchurl {
      url = "https://github.com/lemonade-sdk/lemonade/releases/download/v${version}/lemonade-server-${version}.x86_64.rpm";
      hash = "sha256-ZU6B8sos7+ewGJ8phkitfNg200w+Ege92D6NSqBDnWE=";
    };

    nativeBuildInputs = with pkgs; [
      rpm
      cpio
      autoPatchelfHook
    ];

    buildInputs = with pkgs; [
      stdenv.cc.cc.lib # libstdc++
      systemd          # libsystemd
      brotli           # libbrotli*
      zlib             # libz
      curl             # libcurl (for CLI)
      libwebsockets    # libwebsockets (for router)
    ];

    # Force libwebsockets into RPATH (autoPatchelfHook finds it but
    # doesn't always add the lib path)
    runtimeDependencies = [ pkgs.libwebsockets ];

    unpackPhase = ''
      rpm2cpio $src | cpio -idm
    '';

    installPhase = ''
      mkdir -p $out/bin $out/share/lemonade-server $out/etc/lemonade

      cp opt/bin/lemonade-server $out/bin/
      cp opt/bin/lemonade $out/bin/
      cp opt/bin/lemonade-router $out/bin/

      # Router expects resources/ next to its binary
      cp -r opt/share/lemonade-server/resources $out/bin/resources
      cp -r opt/share/lemonade-server/resources $out/share/lemonade-server/
      cp etc/lemonade/lemonade.conf $out/etc/lemonade/
    '';
  };
in
{
  environment.systemPackages = [ lemonade-server ];

  systemd.services.lemonade = {
    description = "Lemonade AI inference server";
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    path = with pkgs; [ procps coreutils bash gnutar gzip ];

    environment = {
      HOME = "/var/lib/lemonade";
      LEMONADE_HOST = "0.0.0.0";
      LEMONADE_PORT = "13305";
      LEMONADE_LOG_LEVEL = "info";
      LEMONADE_CTX_SIZE = "131072";
      LEMONADE_LLAMACPP = "rocm";
      LEMONADE_LLAMACPP_PREFER_SYSTEM = "1";
      LEMONADE_LLAMACPP_ROCM_BIN = "${llama-cpp-rocm-gemma4}/bin/llama-server";
      LEMONADE_LLAMACPP_ARGS = "--flash-attn enabled --cache-type-k q8_0 --cache-type-v q8_0";
    };

    serviceConfig = {
      Type = "simple";
      ExecStart = "${lemonade-server}/bin/lemonade-server serve";
      Restart = "on-failure";
      RestartSec = 5;
      KillSignal = "SIGINT";

      # Lemonade stores downloaded models and backends in its working dir
      StateDirectory = "lemonade";
      CacheDirectory = "lemonade";
      LogsDirectory = "lemonade";
      WorkingDirectory = "/var/lib/lemonade";

      ReadWritePaths = [ "/var/lib/lemonade" "/var/cache/lemonade" ];

      # GPU access needed for Vulkan/ROCm
      PrivateDevices = false;
      SupplementaryGroups = [ "video" "render" ];

      # Hardening
      DynamicUser = true;
      CapabilityBoundingSet = "";
      AmbientCapabilities = "CAP_SYS_RESOURCE";
      RestrictAddressFamilies = [
        "AF_INET"
        "AF_INET6"
        "AF_UNIX"
      ];
      NoNewPrivileges = true;
      PrivateMounts = true;
      PrivateTmp = true;
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectHome = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectSystem = "strict";
      LockPersonality = true;
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      SystemCallArchitectures = "native";
      SystemCallFilter = [
        "@system-service"
        "~@privileged"
      ];
      SystemCallErrorNumber = "EPERM";
      ProtectProc = "default";
      ProtectHostname = true;
      ProcSubset = "all";
    };
  };
}
