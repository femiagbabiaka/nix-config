# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, inputs, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "cerebro"; # Define your hostname.

  # Configure network connections interactively with nmcli or nmtui.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  nixpkgs.config.allowUnfree = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.femi = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [
      fish
      zoxide
      delta
      bat
      fzf
      fastfetch
      htop
    ];
  };

  users.users.tramp = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    shell = pkgs.bash;
    packages = with pkgs; [
      git
    ];
  };

  environment.systemPackages = with pkgs; [
    inputs.llm-agents.packages.x86_64-linux.claude-code
    cifs-utils
    delve
    git
    go
    gopls
    gotools
    libdrm
    racket
    ripgrep
    rocmPackages.amdsmi
    rustup
    helix # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  ];

  services.prowlarr.enable = true;
  # Prowlarr listens on 127.0.0.1:9696 by default

  services.plex = {
    enable = true;
    dataDir = "/var/lib/plex";
    openFirewall = true;
  };

  systemd.services.llama-cpp = {
    description = "LLaMA C++ server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "idle";
      KillSignal = "SIGINT";
      ExecStart = "${pkgs.llama-cpp-vulkan}/bin/llama-server --log-disable --host 0.0.0.0 --port 8081 -hf unsloth/Qwen3-30B-A3B-Instruct-2507-GGUF:Q8_0 --jinja -ngl 99 --threads -1 --ctx-size 262144     --temp 0.7 --min-p 0.0 --top-p 0.80 --top-k 20 --presence-penalty 1.0";
      Restart = "on-failure";
      RestartSec = 300;

      ReadWritePaths = [ "/models" ];

      # Give the service proper writable homes (created with correct perms)
      StateDirectory = "llama-cpp";
      CacheDirectory = "llama-cpp";
      LogsDirectory  = "llama-cpp";

      # Make XDG paths explicit so the app doesn’t guess dumb locations
      Environment = [
        "XDG_STATE_HOME=/var/lib/llama-cpp"
        "XDG_CACHE_HOME=/var/cache/llama-cpp"
        "XDG_DATA_HOME=/var/lib/llama-cpp"
        "XDG_RUNTIME_DIR=/run/llama-cpp"
      ];

      # for GPU acceleration
      PrivateDevices = false;

      # hardening
      DynamicUser = true;
      CapabilityBoundingSet = "";
      RestrictAddressFamilies = [
        "AF_INET"
        "AF_INET6"
        "AF_UNIX"
      ];
      NoNewPrivileges = true;
      PrivateMounts = true;
      PrivateTmp = true;
      PrivateUsers = true;
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectHome = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectSystem = "strict";
      MemoryDenyWriteExecute = true;
      LockPersonality = true;
      RemoveIPC = true;
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

  programs.fish.enable = true;

  # List services that you want to enable:
  # Enable common container config files in /etc/containers
  virtualisation.containers.enable = true;
  virtualisation = {
    podman = {
      enable = true;

      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = true;

      # Required for containers under podman-compose to be able to talk to each other.
      defaultNetwork.settings.dns_enabled = true;
    };
  };

  services.tailscale = {
    enable = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  programs.mosh.enable = true;



  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "26.05"; # Did you read the comment?

}
