#
# ~/.bashrc
#
export GOPATH=$HOME/go
export PATH="/home/oliver/.local/bin/:/home/oliver/scripts/:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:$GOPATH/bin"

# ssh agent startup
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! -f "$SSH_AUTH_SOCK" ]]; then
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls -a --color=auto'
alias gandissh='ssh -oHostKeyAlgorithms=+ssh-rsa 8926677@console.sd3.gpaas.net'
alias gandideploy='ssh -oHostKeyAlgorithms=+ssh-rsa 8926677@git.sd3.gpaas.net deploy default.git'
alias unrealme='$HOME/projects/UnrealEngine/Engine/Binaries/Linux/UnrealEditor'
alias hogwarts='echo sysctl -w vm.max_map_count=1048576'

PS1='[\u@\h \W]\$ '
