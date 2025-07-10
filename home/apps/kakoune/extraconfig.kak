# set up vsplit and hsplit
# Define vertical split command
define-command kitty-vsplit -params .. %{
    evaluate-commands %sh{
        if [ -n "$KITTY_WINDOW_ID" ]; then
            if [ $# -eq 0 ]; then
                # No file specified, open current buffer
                echo "nop %sh{ kitty @ launch --no-response --type=window --location=vsplit --cwd=current kak -c $kak_session '$kak_buffile' }"
            else
                # File specified
                echo "nop %sh{ kitty @ launch --no-response --type=window --location=vsplit --cwd=current kak -c $kak_session '$@' }"
            fi
        else
            # Fallback if not in kitty
            if [ $# -eq 0 ]; then
                echo "echo 'Not running in kitty'"
            else
                echo "edit %arg{@}"
            fi
        fi
    }
}

# Define horizontal split command
define-command kitty-hsplit -params .. %{
    evaluate-commands %sh{
        if [ -n "$KITTY_WINDOW_ID" ]; then
            if [ $# -eq 0 ]; then
                # No file specified, open current buffer
                echo "nop %sh{ kitty @ launch --no-response --type=window --location=hsplit --cwd=current kak -c $kak_session '$kak_buffile' }"
            else
                # File specified
                echo "nop %sh{ kitty @ launch --no-response --type=window --location=hsplit --cwd=current kak -c $kak_session '$@' }"
            fi
        else
            # Fallback if not in kitty
            if [ $# -eq 0 ]; then
                echo "echo 'Not running in kitty'"
            else
                echo "edit %arg{@}"
            fi
        fi
    }
}

# Convenient aliases
alias global vs kitty-vsplit
alias global sp kitty-hsplit

# map splits in user mode
map global user | ':kitty-vsplit <ret>' -docstring 'split window vertically'
map global user <minus> ':kitty-hsplit <ret>' -docstring 'split window horizontally'
# smarttab.kak
hook global BufOpenFile .* expandtab
hook global BufNewFile  .* expandtab
hook global WinSetOption filetype=(ruby|yaml|json) %{
    set-option global indentwidth 2
    set-option global softtabstop 2
}
hook global ModuleLoaded smarttab %{
    expandtab
    set-option global indentwidth 4
    set-option global softtabstop 4
    # you can configure text that is being used to represent curent active mode
    set-option global smarttab_expandtab_mode_name 'exp'
    set-option global smarttab_noexpandtab_mode_name 'noexp'
    set-option global smarttab_smarttab_mode_name 'smart'
}
# LSP
eval %sh{kak-lsp}
# extra lsp keybindings
map global user l ':enter-user-mode lsp<ret>' -docstring 'LSP mode'
map global insert <tab> '<a-;>:try lsp-snippets-select-next-placeholders catch %{ execute-keys -with-hooks <lt>tab> }<ret>' -docstring 'Select next snippet placeholder'
map global object a '<a-semicolon>lsp-object<ret>' -docstring 'LSP any symbol'
map global object <a-a> '<a-semicolon>lsp-object<ret>' -docstring 'LSP any symbol'
map global object f '<a-semicolon>lsp-object Function Method<ret>' -docstring 'LSP function or method'
map global object t '<a-semicolon>lsp-object Class Interface Struct<ret>' -docstring 'LSP class interface or struct'
map global object d '<a-semicolon>lsp-diagnostic-object --include-warnings<ret>' -docstring 'LSP errors and warnings'
map global object D '<a-semicolon>lsp-diagnostic-object<ret>' -docstring 'LSP errors'

# map formatting to lsp specific formatting
alias global format lsp-formatting
alias global format-buffer lsp-formatting
alias global format-selections lsp-range-formatting

# use the lsp setting to format the buffer before writing it
hook global BufSetOption filetype=.* %{
    hook buffer BufWritePre .* lsp-formatting-sync
}

# set up language specific lsp configuration
hook global BufSetOption filetype=nix %{
    set-option buffer lsp_servers %{
        [nil]
        root_globs = ["flake.nix", "shell.nix", ".git", ".hg"]
        [nil.settings]
        nil.formatting.command = ["nixfmt"]
    }
}

hook global BufSetOption filetype=go %{
    set-option buffer lsp_servers %{
        [gopls]
        root_globs = ["Gopkg.toml", "go.mod", ".git", ".hg"]
        [gopls.settings.gopls]
        # See https://github.com/golang/tools/blob/master/gopls/doc/settings.md
        # "build.buildFlags" = []
        hints.assignVariableTypes = true
        hints.compositeLiteralFields = true
        hints.compositeLiteralTypes = true
        hints.constantValues = true
        hints.functionTypeParameters = true
        hints.parameterNames = true
        hints.rangeVariableTypes = true
        usePlaceholders = true
        "formatting.gofumpt" = true
    }
}
lsp-enable

# set up autopairs
enable-auto-pairs

# set up fzf.kak
map global user f ': fzf-mode<ret>' -docstring 'trigger fzf-mode'
hook global ModuleLoaded fzf-file %{
	set-option global fzf_file_command 'rg'
	set-option global fzf_highlight_command 'bat'
}
