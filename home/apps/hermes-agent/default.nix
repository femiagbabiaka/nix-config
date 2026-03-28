{
  pkgs,
  lib,
  hostname,
  ...
}:
let
  python3Packages = pkgs.python3Packages;

  parallel-web = python3Packages.buildPythonPackage rec {
    pname = "parallel-web";
    version = "0.4.2";
    src = pkgs.fetchPypi {
      pname = "parallel_web";
      inherit version;
      hash = "sha256-WZtajzh9w1x9yMgeNy6t9pWKQKys6li/Fw38ZjwAPac=";
    };
    pyproject = true;
    build-system = with python3Packages; [
      hatchling
      hatch-fancy-pypi-readme
    ];
    postPatch = ''
      substituteInPlace pyproject.toml \
        --replace-fail 'requires = ["hatchling==1.26.3"' 'requires = ["hatchling"'
    '';
    dependencies = with python3Packages; [
      httpx
      pydantic
      typing-extensions
      anyio
      distro
      sniffio
    ];
    doCheck = false;
  };

  fal-client = python3Packages.buildPythonPackage rec {
    pname = "fal-client";
    version = "0.13.2";
    src = pkgs.fetchPypi {
      pname = "fal_client";
      inherit version;
      hash = "sha256-1UPU/0nyfYS8A4Ur0caqNDYE4H1e0xud941iXgEDCMs=";
    };
    pyproject = true;
    build-system = with python3Packages; [
      setuptools
      setuptools-scm
    ];
    dependencies = with python3Packages; [
      httpx
      httpx-sse
      msgpack
      websockets
    ];
    doCheck = false;
  };

  honcho-ai = python3Packages.buildPythonPackage rec {
    pname = "honcho-ai";
    version = "2.0.1";
    src = pkgs.fetchPypi {
      pname = "honcho_ai";
      inherit version;
      hash = "sha256-b97r+UVOYrxSPVeIjlA1nme6r9sh9oYh+cFOCNwAYjo=";
    };
    pyproject = true;
    build-system = [ python3Packages.setuptools ];
    dependencies = with python3Packages; [
      httpx
      pydantic
      typing-extensions
    ];
    doCheck = false;
  };

  agent-browser = pkgs.stdenv.mkDerivation rec {
    pname = "agent-browser";
    version = "0.22.3";
    src = pkgs.fetchurl {
      url = "https://registry.npmjs.org/agent-browser/-/agent-browser-${version}.tgz";
      hash = "sha256-hzHQ+yqoVpe+7tsxDnBrS3tmJk0aVQ80yCQGT5Il6Wk=";
    };
    sourceRoot = "package";
    nativeBuildInputs = [
      pkgs.autoPatchelfHook
      pkgs.makeWrapper
    ];
    installPhase = ''
      mkdir -p $out/bin
      install -m 0755 bin/agent-browser-linux-x64 $out/bin/agent-browser
    '';
    postFixup = ''
      wrapProgram $out/bin/agent-browser \
        --set AGENT_BROWSER_EXECUTABLE_PATH ${pkgs.chromium}/bin/chromium \
        --set AGENT_BROWSER_ARGS "--no-sandbox,--disable-gpu,--disable-dev-shm-usage,--no-zygote,--single-process"
    '';
    meta.description = "Headless browser automation CLI for AI agents";
  };

  hermes-agent = python3Packages.buildPythonApplication rec {
    pname = "hermes-agent";
    version = "0.4.0";
    src = pkgs.fetchFromGitHub {
      owner = "NousResearch";
      repo = "hermes-agent";
      rev = "8416bc2142ad7494b3d72b055cd5a86a80472fe4";
      hash = "sha256-ATOm0aJbE7W4q/shheXMIzeVNTjpySDnT5Pk/RxoNPY=";
    };
    pyproject = true;
    build-system = [ python3Packages.setuptools ];
    postPatch = ''
      substituteInPlace gateway/platforms/signal.py \
        --replace-fail \
          'from urllib.parse import unquote' \
          'from urllib.parse import unquote, quote'
      substituteInPlace gateway/platforms/signal.py \
        --replace-fail \
          'url = f"{self.http_url}/api/v1/events?account={self.account}"' \
          'url = f"{self.http_url}/api/v1/events?account={quote(self.account)}"'

      # Fix Python 3.13 argparse bug: nested subparsers may leave dest=None
      # even when a valid subcommand matched. Check func before the fallback.
      sed -i 's/if args.command is None:/if args.command is None and not hasattr(args, "func"):/' \
        hermes_cli/main.py
    '';
    dependencies = with python3Packages; [
      openai
      anthropic
      python-dotenv
      fire
      httpx
      rich
      tenacity
      pyyaml
      requests
      jinja2
      pydantic
      prompt-toolkit
      firecrawl-py
      parallel-web
      fal-client
      edge-tts
      faster-whisper
      litellm
      typer
      platformdirs
      pyjwt
      # [cron]
      croniter
      # [honcho]
      honcho-ai
      # [mcp]
      mcp
      # [messaging]
      python-telegram-bot
      discordpy
      aiohttp
      slack-bolt
      slack-sdk
      # [cli]
      simple-term-menu
      # [tts-premium]
      elevenlabs
      # [pty]
      ptyprocess
      # [homeassistant] / [sms]
      # (aiohttp already listed above)
      # [acp]
      agent-client-protocol
      # [voice]
      sounddevice
      numpy
      # [matrix]
      matrix-nio
      # [slack] (already covered by messaging)
      # [dev]
      pytest
      pytest-asyncio
      pytest-xdist
    ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postFixup = ''
      for bin in $out/bin/*; do
        wrapProgram "$bin" \
          --prefix PATH : ${lib.makeBinPath [ pkgs.ripgrep pkgs.ffmpeg pkgs.nodejs_22 pkgs.signal-cli agent-browser pkgs.uv pkgs.chromium ]} \
          --set SIGNAL_HTTP_URL http://localhost:8080
      done
    '';
    doCheck = false;
    meta = {
      description = "Self-improving AI agent by Nous Research";
      homepage = "https://github.com/NousResearch/hermes-agent";
      license = lib.licenses.asl20;
    };
  };
in
{
  home.packages = [ hermes-agent ];

  systemd.user.services.signal-cli-daemon = lib.mkIf (hostname == "cerebro") {
    Unit = {
      Description = "signal-cli JSON-RPC daemon";
      After = [ "network.target" ];
    };
    Service = {
      ExecStart = "${pkgs.signal-cli}/bin/signal-cli --config %h/.local/share/signal-cli daemon --http localhost:8080";
      Restart = "always";
      RestartSec = "5";
    };
    Install.WantedBy = [ "default.target" ];
  };

  systemd.user.services.hermes-gateway = lib.mkIf (hostname == "cerebro") {
    Unit = {
      Description = "Hermes Agent Gateway - Messaging Platform Integration";
      After = [ "network.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${hermes-agent}/bin/hermes gateway run --replace";
      Environment = [ "HERMES_HOME=%h/.hermes" ];
      EnvironmentFile = [ "%h/.config/hermes-gateway/env" ];
      Restart = "on-failure";
      RestartSec = "30";
      KillMode = "mixed";
      KillSignal = "SIGTERM";
      TimeoutStopSec = "60";

      # Hardening
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectHostname = true;
      LockPersonality = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      SystemCallArchitectures = "native";
      SystemCallFilter = [ "@system-service" "~@privileged" ];
      SystemCallErrorNumber = "EPERM";
    };
    Install.WantedBy = [ "default.target" ];
  };
}
