{ config, pkgs, lib, hostname, ... }:

lib.mkIf (hostname == "cerebro") {
  systemd.user.services.torrentbox-mcp = {
    Unit = {
      Description = "torrentbox MCP server";
      After = [ "network.target" ];
    };
    Service = {
      ExecStart = "${pkgs.uv}/bin/uv run --project %h/src/torrentbox-mcp torrentbox-mcp";
      Restart = "always";
      RestartSec = "5";
      WorkingDirectory = "%h/src/torrentbox-mcp";
      Environment = [
        "PATH=${lib.makeBinPath [ pkgs.openssh pkgs.rsync pkgs.uv pkgs.python313 ]}:%h/.local/bin:/run/current-system/sw/bin"
        "UV_PYTHON=${pkgs.python313}/bin/python3"
      ];
    };
    Install.WantedBy = [ "default.target" ];
  };

  systemd.user.services.poke-tunnel-torrentbox = {
    Unit = {
      Description = "Poke tunnel for torrentbox MCP";
      After = [
        "torrentbox-mcp.service"
        "network.target"
      ];
      Requires = [ "torrentbox-mcp.service" ];
    };
    Service = {
      ExecStart = "${pkgs.nodejs_25}/bin/npx --yes poke tunnel http://localhost:8001/mcp --name \"torrentbox\"";
      Restart = "always";
      RestartSec = "15";
      EnvironmentFile = [ "%h/.config/torrentbox/poke.env" ];
      Environment = [
        "PATH=${lib.makeBinPath [ pkgs.nodejs_25 ]}:/run/current-system/sw/bin"
        "HOME=%h"
      ];
    };
    Install.WantedBy = [ "default.target" ];
  };
}
