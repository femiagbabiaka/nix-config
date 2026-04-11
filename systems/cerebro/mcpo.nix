{ pkgs, ... }:

let
  python3Packages = pkgs.python3Packages;

  mcpo = python3Packages.buildPythonApplication rec {
    pname = "mcpo";
    version = "0.0.20";
    pyproject = true;

    src = pkgs.fetchPypi {
      inherit pname version;
      hash = "sha256-vBksh3Wqn0bs8+hBgGRAefiDK17Z8SoKocueJyjgWQw=";
    };

    build-system = [ python3Packages.hatchling ];

    dependencies = with python3Packages; [
      click
      fastapi
      mcp
      passlib
      pydantic
      pyjwt
      python-dotenv
      typer
      uvicorn
      watchdog
    ];

    doCheck = false;
  };

  mcpoConfig = pkgs.writeText "mcpo-config.json" (builtins.toJSON {
    mcpServers = {
      torrentbox = {
        type = "streamable-http";
        url = "http://127.0.0.1:8001/mcp";
      };
    };
  });
in
{
  systemd.services.mcpo = {
    description = "mcpo - MCP to OpenAPI proxy";
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "simple";
      ExecStart = "${mcpo}/bin/mcpo --port 8003 --host 0.0.0.0 --config ${mcpoConfig}";
      Restart = "on-failure";
      RestartSec = 5;

      DynamicUser = true;
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectHostname = true;
      LockPersonality = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
      SystemCallArchitectures = "native";
      SystemCallFilter = [ "@system-service" "~@privileged" ];
      SystemCallErrorNumber = "EPERM";
    };
  };
}
