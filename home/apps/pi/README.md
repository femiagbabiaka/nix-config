# Pi Configuration Module

Home Manager module for configuring [pi](https://github.com/badlogic/pi-mono), the minimal terminal coding harness.

## Features

- Install `pi-coding-agent` package
- Manage `~/.pi/agent/settings.json`
- Configure global `AGENTS.md` with project instructions
- Set custom system prompts (`SYSTEM.md`, `APPEND_SYSTEM.md`)
- Enable **built-in extensions** from the examples package
- Organize custom extensions, skills, prompts, and themes

## Usage

### Basic Usage

Enable the module and configure pi:

```nix
{
  imports = [ ./home/apps/pi ];

  programs.pi = {
    enable = true;
    settings = {
      defaultProvider = "anthropic";
      defaultModel = "claude-sonnet-4-20250514";
      defaultThinkingLevel = "medium";
      hideThinkingBlock = true;
    };
  };
}
```

### Full Configuration Options

```nix
programs.pi = {
  enable = true;

  # Pi settings (written to ~/.pi/agent/settings.json)
  settings = {
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
    
    retry = {
      enabled = true;
      maxRetries = 3;
    };
    
    # See https://github.com/badlogic/pi-mono/blob/main/packages/pi-coding-agent/docs/settings.md for all options
  };

  # Global AGENTS.md for project instructions
  agentsMd = ''
    # My Development Environment

    ## Common Commands
    - `nix flake check` - Validate configuration
    - `nix build .#myapp` - Build my application
  '';

  # Custom system prompt (replaces default)
  systemMd = null;

  # Append to system prompt
  appendSystemMd = ''
    ## Additional Instructions
    Always use two-space indentation for Nix code.
  '';

  # Custom extensions (paths to symlink)
  extensions = [ ./my-custom-extension ];

  # Enable built-in extensions from pi-coding-agent examples package
  builtInExtensions = {
    enable = true;
    
    # Safety extensions
    permissionGate = true;        # Confirm before dangerous bash commands (rm -rf, sudo)
    protectedPaths = true;        # Block writes to .env, .git/, node_modules/
    confirmDestructive = true;    # Confirm before clear/switch/fork
    dirtyRepoGuard = true;        # Prevent changes with uncommitted git changes
    
    # Productivity extensions
    todo = true;                  # Todo list tool with /todos command
    gitCheckpoint = false;        # Git stash checkpoints at each turn
    autoCommitOnExit = false;     # Auto-commit on exit
    
    # UI/UX extensions
    notify = true;                # Desktop notifications when agent finishes
    summarize = true;             # Summarize conversation with GPT-5.2
    statusLine = true;            # Show turn progress in footer
    titlebarSpinner = true;       # Braille spinner in terminal title
    modelStatus = true;           # Show model changes in status bar
    customFooter = true;          # Custom footer with git branch and token stats
    
    # Interactive features
    inlineBash = true;            # Expand !{command} patterns in prompts
    interactiveShell = false;     # Run interactive commands (vim, htop)
    
    # Custom tools
    tools = true;                 # Interactive /tools command to enable/disable tools
    handoff = false;              # Transfer context via /handoff <goal>
    qna = false;                  # Extract questions from responses
    
    # System prompt & compaction
    claudeRules = true;           # Scan .claude/rules/ folder
    triggerCompact = true;        # Auto-compact at 100k tokens + /trigger-compact
    
    # Session management
    sessionName = false;          # Name sessions for selector
    bookmark = false;             # Bookmark entries for /tree navigation
    
    # Other useful extensions
    preset = false;               # Named presets via --preset flag
    shutdownCommand = false;      # Adds /quit command
    
    # Games (for fun)
    snake = false;                # Snake game
    
    # Advanced features
    ssh = false;                  # Delegate tools to remote machine via SSH
  };

  # Custom skills
  skills = [ ./my-custom-skill ];

  # Custom prompt templates
  prompts = [ ./my-prompts ];

  # Custom themes
  themes = [ ./my-theme ];
}
```

## Built-in Extensions

The module provides easy access to extensions from the [pi-coding-agent examples](https://github.com/badlogic/pi-mono/blob/main/packages/coding-agent/examples/extensions/README.md). Enable them via `builtInExtensions`:

### Safety Extensions

- `permissionGate` - Prompts for confirmation before dangerous bash commands (rm -rf, sudo, etc.)
- `protectedPaths` - Blocks writes to protected paths (.env, .git/, node_modules/)
- `confirmDestructive` - Confirms before destructive session actions (clear, switch, fork)
- `dirtyRepoGuard` - Prevents session changes with uncommitted git changes

### Productivity Extensions

- `todo` - Todo list tool with `/todos` command and custom rendering with state persistence
- `gitCheckpoint` - Creates git stash checkpoints at each turn for code restoration on fork
- `autoCommitOnExit` - Auto-commits on exit using last assistant message for commit message

### UI/UX Extensions

- `notify` - Desktop notifications via OSC 777 when agent finishes (Ghostty, iTerm2, WezTerm)
- `summarize` - Summarize conversation with GPT-5.2 and show in transient UI
- `statusLine` - Shows turn progress in footer via `ctx.ui.setStatus()`
- `titlebarSpinner` - Braille spinner animation in terminal title while the agent is working
- `modelStatus` - Shows model changes in status bar via `model_select` hook
- `customFooter` - Custom footer with git branch and token stats via `ctx.ui.setFooter()`

### Interactive Features

- `inlineBash` - Expands `!{command}` patterns in prompts via `input` event transformation
- `interactiveShell` - Run interactive commands (vim, htop) with full terminal via `user_bash` hook

### Custom Tools

- `tools` - Interactive `/tools` command to enable/disable tools with session persistence
- `handoff` - Transfer context to a new focused session via `/handoff <goal>`
- `qna` - Extracts questions from last response into editor via `ctx.ui.setEditorText()`

### System Prompt & Compaction

- `claudeRules` - Scans `.claude/rules/` folder and lists rules in system prompt
- `triggerCompact` - Triggers compaction when context usage exceeds 100k tokens and adds `/trigger-compact` command

### Session Management

- `sessionName` - Name sessions for the session selector via `setSessionName`
- `bookmark` - Bookmark entries with labels for `/tree` navigation via `setLabel`

### Other Extensions

- `preset` - Named presets for model, thinking level, tools, and instructions via `--preset` flag
- `shutdownCommand` - Adds `/quit` command demonstrating `ctx.shutdown()`
- `snake` - Snake game with custom UI, keyboard handling, and session persistence
- `ssh` - Delegate all tools to a remote machine via SSH using pluggable operations

### Recommended Setup

```nix
programs.pi.builtInExtensions = {
  enable = true;
  
  # Safety first
  permissionGate = true;
  protectedPaths = true;
  
  # Productivity boosters
  todo = true;
  
  # Nice-to-have UI
  notify = true;
  summarize = true;
  statusLine = true;
  titlebarSpinner = true;
  
  # Useful features
  inlineBash = true;
  tools = true;
  triggerCompact = true;
};
```

## Settings Reference

All settings from [pi's settings documentation](https://github.com/badlogic/pi-mono/blob/main/packages/pi-coding-agent/docs/settings.md) are supported:

### Model & Thinking
- `defaultProvider`: Default LLM provider
- `defaultModel`: Default model ID
- `defaultThinkingLevel`: Thinking budget level
- `hideThinkingBlock`: Hide thinking in output
- `thinkingBudgets`: Custom token budgets per level

### UI & Display
- `theme`: Color theme
- `quietStartup`: Hide startup header
- `collapseChangelog`: Condense changelog
- `editorPaddingX`: Input editor padding

### Compaction
- `compaction.enabled`: Enable auto-compaction
- `compaction.reserveTokens`: Tokens reserved for response
- `compaction.keepRecentTokens`: Recent tokens to keep

### Retry
- `retry.enabled`: Enable automatic retry
- `retry.maxRetries`: Maximum retry attempts
- `retry.baseDelayMs`: Base delay for backoff
- `retry.maxDelayMs`: Maximum server delay

## Directory Structure

The module manages files in `~/.pi/agent/`:

```
~/.pi/agent/
├── settings.json          # Managed by module (if settings configured)
├── AGENTS.md              # Managed by module (if agentsMd configured)
├── SYSTEM.md              # Managed by module (if systemMd configured)
├── APPEND_SYSTEM.md       # Managed by module (if appendSystemMd configured)
├── extensions/            # Custom extensions
├── skills/                # Custom skills
├── prompts/               # Custom prompt templates
└── themes/                # Custom themes
```

## Examples

### Lemonade Provider (Local GGUF Models)

```nix
programs.pi.settings = {
  defaultProvider = "lemonade";
  defaultModel = "Qwen3.5-27B-GGUF";
  hideThinkingBlock = true;
  defaultThinkingLevel = "medium";
};
```

### Anthropic Claude

```nix
programs.pi.settings = {
  defaultProvider = "anthropic";
  defaultModel = "claude-sonnet-4-20250514";
  defaultThinkingLevel = "high";
};
```

### OpenAI

```nix
programs.pi.settings = {
  defaultProvider = "openai";
  defaultModel = "gpt-4o";
};
```

## Notes

- Settings are written as JSON to `~/.pi/agent/settings.json`
- The module only creates files when values are explicitly configured
- Set values to `null` or `{}` to disable management of that file
- For project-specific settings, use `.pi/settings.json` in your project directory

## References

- [Pi Documentation](https://github.com/badlogic/pi-mono)
- [Settings Reference](https://github.com/badlogic/pi-mono/blob/main/packages/pi-coding-agent/docs/settings.md)
- [Extensions Guide](https://github.com/badlogic/pi-mono/blob/main/packages/pi-coding-agent/docs/extensions.md)
- [Skills Guide](https://github.com/badlogic/pi-mono/blob/main/packages/pi-coding-agent/docs/skills.md)
