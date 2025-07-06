set -g fish_greeting ""
zoxide init fish --cmd cd | source

set -x LSP_USE_LISTS true
set -x EDITOR kak
set -l NON_WORK_HOSTS laincomp nixos cassiopeia jormungand tachibana

if not contains (hostname) $NON_WORK_HOSTS
    eval (cinc shell-init fish)
    source ~/.kryten/fish-env
    export KITCHEN_LOCAL_YAML=.kitchen.gce.yml
    set -x FASTLY_CHEF_USERNAME fagbabiaka
    set -x GOOGLE_SSH_USERNAME fagbabiaka
    set -x INFRA_SERVER infra.plat.k8s.secretcdn.net
    set -x INFRA_PROVIDER okta
    set -x INFRA_SKIP_VERSION_CHECK true
    set -x PATH ~/.config/emacs/bin $PATH
    set -x PATH ~/.rd/bin $PATH
    set -x -a PATH /usr/local/go/bin
    set -x -a PATH ~/go/bin
end

set -g fish_greeting ""
set -U fish_color_autosuggestion brblack
set -U fish_color_cancel -r
set -U fish_color_command green
set -U fish_color_comment magenta
set -U fish_color_cwd green
set -U fish_color_cwd_root red
set -U fish_color_end magenta
set -U fish_color_error red
set -U fish_color_escape cyan
set -U fish_color_history_current --bold
set -U fish_color_host normal
set -U fish_color_normal normal
set -U fish_color_operator cyan
set -U fish_color_param blue
set -U fish_color_quote yellow
set -U fish_color_redirection yellow
set -U fish_color_search_match yellow '--background=brightblack'
set -U fish_color_selection white --bold '--background=brightblack'
set -U fish_color_status red
set -U fish_color_user green
set -U fish_color_valid_path --underline
set -U fish_pager_color_completion normal
set -U fish_pager_color_description yellow
set -U fish_pager_color_prefix white --bold --underline
set -U fish_pager_color_progress white '--background=cyan'

# prompt
set fish_prompt_pwd_dir_length 1
set __fish_git_prompt_show_informative_status 1

set fish_color_command green
set fish_color_param $fish_color_normal

set __fish_git_prompt_showdirtystate yes
set __fish_git_prompt_showupstream yes

set __fish_git_prompt_color_branch brown
set __fish_git_prompt_color_dirtystate red
set __fish_git_prompt_color_stagedstate yellow
set __fish_git_prompt_color_upstream cyan
set __fish_git_prompt_color_cleanstate green
set __fish_git_prompt_color_invalidstate red

set __fish_git_prompt_char_dirtystate ' ✕ '
set __fish_git_prompt_char_stateseparator ' '
set __fish_git_prompt_char_untrackedfiles '++'
set __fish_git_prompt_char_cleanstate ' ✓ '
set __fish_git_prompt_char_stagedstate '-> '
set __fish_git_prompt_char_conflictedstate "✕ "

set __fish_git_prompt_char_upstream_prefix ""
set __fish_git_prompt_char_upstream_equal ""
set __fish_git_prompt_char_upstream_ahead ' >= '
set __fish_git_prompt_char_upstream_behind ' <= '
set __fish_git_prompt_char_upstream_diverged ' <=> '

function _print_in_color
    set -l string $argv[1]
    set -l color $argv[2]

    set_color $color
    printf $string
    set_color normal
end

function _prompt_color_for_status
    if test $argv[1] -eq 0
        echo magenta
    else
        echo red
    end
end

function fish_jj_prompt
    # If jj isn't installed, there's nothing we can do
    # Return 1 so the calling prompt can deal with it
    if not command -sq jj
        return 1
    end
    set -l info "$(
        jj log 2>/dev/null --no-graph --ignore-working-copy --color=always --revisions @ --template '
            surround(
                "(",
                ")",
                separate(" ",
                    bookmarks,
                    if(conflict, label("conflict", "conflict")),
                    if(divergent, label("divergent", "divergent")),
                    if(parents.len() > 1, label("merge", "merged")),
                    coalesce(
                        if(
                            empty,
                            label("empty", "empty"),
                        ),
                        label("change", "change"),
                    ),
                )
            )
        '
    )"
    or return 1
    if test -n "$info"
        printf ' %s' $info
    end
end

function fish_vcs_prompt --description "Print all vcs prompts"
    # If a prompt succeeded, we assume that it's printed the correct info.
    # This is so we don't try svn if git already worked.
    fish_jj_prompt $argv
    or fish_git_prompt $argv
    or fish_hg_prompt $argv
    or fish_fossil_prompt $argv
    # The svn prompt is disabled by default because it's quite slow on common svn repositories.
    # To enable it uncomment it.
    # You can also only use it in specific directories by checking $PWD.
    # or fish_svn_prompt
end

function fish_prompt
    set -l last_status $status

    set -l nix_shell_info (
	if test -n "$IN_NIX_SHELL"
		echo -n " [nix-shell] "
	end
	)

    if test -n "$SSH_CONNECTION"
        printf (prompt_login)
    end

    if test $HOME != $PWD
        _print_in_color " "(prompt_pwd) blue
    end
    fish_vcs_prompt

    _print_in_color "$nix_shell_info λ " (_prompt_color_for_status $last_status)
end
