{ config, pkgs, lib, ... }:

{
  # 1. Place the config file in a known location
  xdg.configFile."kanata/hhkb.kbd".source = ./hhkb_kanata.kbd;

  # 2. macOS Service (Launchd)
  launchd.agents.kanata = lib.mkIf pkgs.stdenv.isDarwin {
    enable = true;
    config = {
      Label = "org.nix-community.home-manager.kanata";
      ProgramArguments = [
        "/usr/bin/sudo"
        "${pkgs.kanata}/bin/kanata"
        "--cfg"
        "${config.xdg.configHome}/kanata/hhkb.kbd"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardOutPath = "/tmp/kanata.out";
      StandardErrorPath = "/tmp/kanata.err";
      ProcessType = "Interactive"; # Sometimes helps with priority
    };
  };

  # 3. Linux Service (Systemd)
  systemd.user.services.kanata = lib.mkIf pkgs.stdenv.isLinux {
    Unit = {
      Description = "Kanata keyboard remapper";
      Documentation = "[https://github.com/jtroo/kanata](https://github.com/jtroo/kanata)";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = "${pkgs.kanata}/bin/kanata --cfg ${config.xdg.configHome}/kanata/hhkb.kbd";
      Restart = "always";
      RestartSec = "5";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
