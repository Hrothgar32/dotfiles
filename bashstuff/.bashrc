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
alias egyetemdown='rclone sync -P -L nextcloud:Egyetem ~/Documents/Egyetem'
alias egyetemup='rclone sync -P -L ~/Documents/Egyetem nextcloud:Egyetem'
export ALTERNATE_EDITOR="emacsclient -c"
export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c"
setxkbmap -layout us,hu,ro -variant ,basic -option grp:alt_shift_toggle
# Created by `pipx` on 2021-08-22 21:46:16
export PATH="$PATH:/home/hrothgar32/.local/bin:$HOME/.ghcup/bin"
