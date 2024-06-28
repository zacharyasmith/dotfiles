export EDITOR="emacsclient -nw -a ''"
alias emt="$EDITOR"


function git-rcheckout ()
{
    ((!$#)) && echo No branch name, command ignored! && exit 1
    git checkout $1 && git submodule foreach --recursive git checkout $1
}

function fg-cpp ()
{
    find -type f \( -name '*.c' -o -name '*.h' -o -name '*.cpp' -o -name '*.hpp' \) -exec grep -Hni $1 \{\} \;
}

function fg-cmakef ()
{
    find -type f \( -name 'CMakeLists.txt' -o -name '*.cmake' \) -exec grep -Hni $1 \{\} \;
}

function fg-any ()
{
    find -type f -exec grep -Hni $1 \{\} \;
}

function fg-glob ()
{
    find -type f -name "${1}" -exec grep -Hni $2 \{\} \;
}

# in your .bashrc/.zshrc/*rc
alias bathelp='bat --plain --language=help'
help() {
    "$@" --help 2>&1 | bathelp
}

export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export BAT_THEME="Visual Studio Dark+"

# [ -f ~/.fzf.bash ] && source ~/.fzf.bash

FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --ansi"
FZF_CTRL_T_COMMAND="bfs -color -mindepth 1 -not \( -name .git \) -printf '%P\n' 2>/dev/null"
FZF_ALT_C_COMMAND="bfs -color -mindepth 1 -not \( -name .git \) -type d -printf '%P\n' 2>/dev/null"

_fzf_compgen_path() {
    bfs -H "$1" -not \( -name .git \) 2>/dev/null
}

_fzf_compgen_dir() {
    bfs -H "$1" -not \( -name .git \) -type d 2>/dev/null
}

_fzf_complete_ssh_notrigger() {
    FZF_COMPLETION_TRIGGER='' _fzf_host_completion
}

# complete -o bashdefault -o default -F _fzf_complete_ssh_notrigger ssh
# complete -o bashdefault -o default -F _fzf_complete_ssh_notrigger mosh
# complete -o bashdefault -o default -F _fzf_complete_ssh_notrigger ss

_fzf_complete_file_notrigger() {
    local prefix=${READLINE_LINE:0:$READLINE_POINT}
    local token=$(sed 's/.* //' <<< "$prefix")
    [ ${#token} -gt 0 ] && prefix=${prefix::-${#token}}

    local selected=$(bfs -H "$token" -type "$1" 2>/dev/null | fzf --height=10 --reverse --border --info=inline --query="$token")
    [ -z "$selected" ] && return

    READLINE_LINE="$prefix$selected${READLINE_LINE:$READLINE_POINT}"
    READLINE_POINT=$(( READLINE_POINT - ${#token} + ${#selected} ))
}

_fzf_complete_dir_notrigger() {
    FZF_COMPLETION_TRIGGER='' _fzf_dir_completion
}

# bind -m emacs-standard -x '"\C-x\C-f": "_fzf_complete_file_notrigger f"'
# bind -m emacs-standard -x '"\C-x\C-d": "_fzf_complete_file_notrigger d"'

export PATH="$PATH:/Users/zach/.cargo/bin"
eval "$(starship init zsh)"
eval "$(/opt/homebrew/bin/brew shellenv)"
