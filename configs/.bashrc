#
# ~/.bashrc
#

export PATH="$HOME/scripts/:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls -a --color=auto'
PS1='[\u@\h \W]\$ '
