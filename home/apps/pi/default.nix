{
  pkgs,
  lib,
  config,
  ...  
}:
let
  cfg = config.programs.pi;
  literalExample = lib.literalExample;
  
  # Get the pi-coding-agent package to access examples
  piPackage = pkgs.pi-coding-agent;
  
  # Find the extensions path (it's nested in node_modules)
  extensionsPath = 
    if builtins.pathExists "${piPackage}/examples/extensions" then
      "${piPackage}/examples/extensions"
    else if builtins.pathExists "${piPackage}/lib/node_modules/pi-monorepo/examples/extensions" then
      "${piPackage}/lib/node_modules/pi-monorepo/examples/extensions"
    else
      null;
  
  # Map extension names to their file paths
  extensionFiles = {
    permissionGate = "permission-gate.ts";
    protectedPaths = "protected-paths.ts";
    confirmDestructive = "confirm-destructive.ts";
    dirtyRepoGuard = "dirty-repo-guard.ts";
    todo = "todo.ts";
    gitCheckpoint = "git-checkpoint.ts";
    autoCommitOnExit = "auto-commit-on-exit.ts";
    notify = "notify.ts";
    summarize = "summarize.ts";
    statusLine = "status-line.ts";
    titlebarSpinner = "titlebar-spinner.ts";
    modelStatus = "model-status.ts";
    customFooter = "custom-footer.ts";
    inlineBash = "inline-bash.ts";
    interactiveShell = "interactive-shell.ts";
    tools = "tools.ts";
    handoff = "handoff.ts";
    qna = "qna.ts";
    claudeRules = "claude-rules.ts";
    triggerCompact = "trigger-compact.ts";
    sessionName = "session-name.ts";
    bookmark = "bookmark.ts";
    preset = "preset.ts";
    shutdownCommand = "shutdown-command.ts";
    snake = "snake.ts";
    ssh = "ssh.ts";
  };
  
  # Generate list of built-in extension paths
  builtInExtensionsList = lib.filter (ext: cfg.builtInExtensions.${ext}) (builtins.attrNames extensionFiles);
  
  # Generate home.files entries for built-in extensions
  builtInExtensionFiles = lib.optionalAttrs (extensionsPath != null) (lib.listToAttrs (map (ext: {
    name = ".pi/agent/extensions/${extensionFiles.${ext}}";
    value = {
      source = "${extensionsPath}/${extensionFiles.${ext}}";
    };
  }) builtInExtensionsList));
  
  # Generate home.files entries for custom extensions
  customExtensionFiles = lib.listToAttrs (map (ext: {
    name = ".pi/agent/extensions/${lib.baseNameOf ext}";
    value = { source = ext; };
  }) cfg.extensions);
in
{
  options.programs.pi = {
    enable = lib.mkEnableOption "Pi AI coding assistant";

    settings = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = ''
        Settings for pi. These will be written to ~/.pi/agent/settings.json.
        See https://github.com/badlogic/pi-mono/blob/main/packages/pi-coding-agent/docs/settings.md
      '';
      example = literalExample ''
        {
          defaultProvider = "anthropic";
          defaultModel = "claude-sonnet-4-20250514";
          defaultThinkingLevel = "medium";
          hideThinkingBlock = true;
          theme = "dark";
          compaction = {
            enabled = true;
            reserveTokens = 16384;
            keepRecentTokens = 20000;
          };
        }
      '';
    };

    agentsMd = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Optional content for ~/.pi/agent/AGENTS.md.
        Use for global project instructions, conventions, and common commands.
        Set to null to disable (default).
      '';
    };

    systemMd = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Optional custom system prompt for ~/.pi/agent/SYSTEM.md.
        Replaces the default system prompt. Set to null to use default (default).
      '';
    };

    appendSystemMd = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Optional text to append to the system prompt via ~/.pi/agent/APPEND_SYSTEM.md.
        Set to null to disable (default).
      '';
    };

    extensions = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [];
      description = ''
        List of paths to custom pi extensions to symlink into ~/.pi/agent/extensions/.
        These are in addition to any built-in extensions selected via `builtInExtensions`.
      '';
    };

    builtInExtensions = lib.mkOption {
      type = lib.types.submodule {
        options = {
          enable = lib.mkEnableOption "Built-in pi extensions from the examples package";
          
          # Safety extensions
          permissionGate = lib.mkEnableOption "Prompts for confirmation before dangerous bash commands (rm -rf, sudo, etc.)";
          protectedPaths = lib.mkEnableOption "Blocks writes to protected paths (.env, .git/, node_modules/)";
          confirmDestructive = lib.mkEnableOption "Confirms before destructive session actions (clear, switch, fork)";
          dirtyRepoGuard = lib.mkEnableOption "Prevents session changes with uncommitted git changes";
          
          # Productivity extensions
          todo = lib.mkEnableOption "Todo list tool with /todos command and custom rendering";
          gitCheckpoint = lib.mkEnableOption "Creates git stash checkpoints at each turn for code restoration on fork";
          autoCommitOnExit = lib.mkEnableOption "Auto-commits on exit using last assistant message for commit message";
          
          # UI/UX extensions
          notify = lib.mkEnableOption "Desktop notifications via OSC 777 when agent finishes (Ghostty, iTerm2, WezTerm)";
          summarize = lib.mkEnableOption "Summarize conversation with GPT-5.2 and show in transient UI";
          statusLine = lib.mkEnableOption "Shows turn progress in footer via ctx.ui.setStatus()";
          titlebarSpinner = lib.mkEnableOption "Braille spinner animation in terminal title while the agent is working";
          modelStatus = lib.mkEnableOption "Shows model changes in status bar via model_select hook";
          customFooter = lib.mkEnableOption "Custom footer with git branch and token stats";
          
          # Interactive features
          inlineBash = lib.mkEnableOption "Expands !{command} patterns in prompts via input event transformation";
          interactiveShell = lib.mkEnableOption "Run interactive commands (vim, htop) with full terminal via user_bash hook";
          
          # Custom tools
          tools = lib.mkEnableOption "Interactive /tools command to enable/disable tools with session persistence";
          handoff = lib.mkEnableOption "Transfer context to a new focused session via /handoff <goal>";
          qna = lib.mkEnableOption "Extracts questions from last response into editor via ctx.ui.setEditorText()";
          
          # System prompt & compaction
          claudeRules = lib.mkEnableOption "Scans .claude/rules/ folder and lists rules in system prompt";
          triggerCompact = lib.mkEnableOption "Triggers compaction when context usage exceeds 100k tokens and adds /trigger-compact command";
          
          # Session management
          sessionName = lib.mkEnableOption "Name sessions for the session selector via setSessionName";
          bookmark = lib.mkEnableOption "Bookmark entries with labels for /tree navigation via setLabel";
          
          # Other useful extensions
          preset = lib.mkEnableOption "Named presets for model, thinking level, tools, and instructions via --preset flag";
          shutdownCommand = lib.mkEnableOption "Adds /quit command demonstrating ctx.shutdown()";
          
          # Games (for fun)
          snake = lib.mkEnableOption "Snake game with custom UI, keyboard handling, and session persistence";
          
          # Advanced features
          ssh = lib.mkEnableOption "Delegate all tools to a remote machine via SSH using pluggable operations";
        };
      };
      default = {};
      description = ''
        Enable built-in extensions from the pi-coding-agent examples package.
        These extensions are automatically copied to ~/.pi/agent/extensions/ when enabled.
        
        See https://github.com/badlogic/pi-mono/blob/main/packages/coding-agent/examples/extensions/README.md
      '';
    };

    skills = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [];
      description = ''
        List of paths to custom pi skills to symlink into ~/.pi/agent/skills/.
      '';
    };

    prompts = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [];
      description = ''
        List of paths to custom pi prompt templates to symlink into ~/.pi/agent/prompts/.
      '';
    };

    themes = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [];
      description = ''
        List of paths to custom pi themes to symlink into ~/.pi/agent/themes/.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.pi-coding-agent ];

    # Build all home.file entries together
    home.file = {
      # Write settings.json if there are custom settings
      ".pi/agent/settings.json" = lib.mkIf (cfg.settings != {}) {
        text = builtins.toJSON cfg.settings;
      };

      # Write AGENTS.md if provided
      ".pi/agent/AGENTS.md" = lib.mkIf (cfg.agentsMd != null) {
        text = cfg.agentsMd;
      };

      # Write SYSTEM.md if provided
      ".pi/agent/SYSTEM.md" = lib.mkIf (cfg.systemMd != null) {
        text = cfg.systemMd;
      };

      # Write APPEND_SYSTEM.md if provided
      ".pi/agent/APPEND_SYSTEM.md" = lib.mkIf (cfg.appendSystemMd != null) {
        text = cfg.appendSystemMd;
      };
    } // builtInExtensionFiles // customExtensionFiles;

    # Create directory structure using home.activation
    home.activation.createPiDirectories = lib.mkIf 
      (cfg.extensions != [] || builtInExtensionsList != [] || cfg.skills != [] || cfg.prompts != [] || cfg.themes != []) ''
      run --quiet mkdir -p "$HOME/.pi/agent/extensions" "$HOME/.pi/agent/skills" "$HOME/.pi/agent/prompts" "$HOME/.pi/agent/themes"
    '';

    # Warn if extensions are enabled but pi package doesn't have examples
    warnings = lib.optional (extensionsPath == null && builtInExtensionsList != []) 
      "pi-coding-agent package doesn't have examples/extensions. Built-in extensions cannot be enabled.";
  };
}
