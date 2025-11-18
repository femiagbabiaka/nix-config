# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

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


  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;




  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

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
      neofetch
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

  users.users.radicle = {
    isSystemUser = true;
    group = "radicle";
    extraGroups = [ "tailscale" ];
    home = "/var/lib/radicle-seed";
    createHome = true;
  };
  users.groups.radicle = {};

  # programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
    delve
    git
    go
    gopls
    gotools
    libdrm
    racket
    radicle-node
    radicle-httpd
    ripgrep
    rocmPackages.amdsmi
    rustup
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  ];

    # 1. The Radicle Node Service (The P2P Engine)
  systemd.services.radicle-seed-node = {
    description = "Radicle Private Seed Node";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" "tailscaled.service" ];
    serviceConfig = {
      User = "radicle";
      Environment = "RAD_HOME=/var/lib/radicle-seed";
      ExecStart = "${pkgs.radicle-node}/bin/radicle-node start";
      Restart = "always";
    };
  };

  # 2. The Radicle HTTP Service (The "Forgejo" Web UI)
  systemd.services.radicle-httpd = {
    description = "Radicle Web Interface";
    wantedBy = [ "multi-user.target" ];
    after = [ "radicle-seed-node.service" "tailscaled.service" ];
    serviceConfig = {
      User = "radicle";
      Environment = "RAD_HOME=/var/lib/radicle-seed";
      # Dynamically determine Tailscale IP at service start
      ExecStart = pkgs.writeShellScript "radicle-httpd-start" ''
        TAILSCALE_IP=$(${pkgs.tailscale}/bin/tailscale ip -4)
        exec ${pkgs.radicle-httpd}/bin/radicle-httpd --listen $TAILSCALE_IP:8081
      '';
      Restart = "always";
    };
  };

  systemd.services.llama-cpp = {
    description = "LLaMA C++ server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "idle";
      KillSignal = "SIGINT";
      ExecStart = "${pkgs.llama-cpp-vulkan}/bin/llama-server --log-disable --host 0.0.0.0 --port 8080 -hf unsloth/Qwen3-30B-A3B-Instruct-2507-GGUF:Q8_0 --jinja -ngl 99 --threads -1 --ctx-size 262144     --temp 0.7 --min-p 0.0 --top-p 0.80 --top-k 20 --presence-penalty 1.0";
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

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
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

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  #system.copySystemConfiguration = true;

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
  system.stateVersion = "25.11"; # Did you read the comment?

}
