{ config, lib, pkgs, ... }:

let
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

    unpackPhase = ''
      rpm2cpio $src | cpio -idm
    '';

    installPhase = ''
      mkdir -p $out/bin $out/share/lemonade-server $out/etc/lemonade

      cp opt/bin/lemonade-server $out/bin/
      cp opt/bin/lemonade $out/bin/
      cp opt/bin/lemonade-router $out/bin/

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

      Environment = [
        "HOME=/var/lib/lemonade"
        "LEMONADE_HOST=0.0.0.0"
        "LEMONADE_PORT=13305"
        "LEMONADE_LOG_LEVEL=info"
        "LEMONADE_CTX_SIZE=131072"
        "LEMONADE_LLAMACPP=rocm"
      ];

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
      ProtectProc = "invisible";
      ProtectHostname = true;
      ProcSubset = "pid";
    };
  };
}
