#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
set -o vi
alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
force_color_prompt=yes
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"
