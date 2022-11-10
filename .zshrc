source ~/.zplug/init.zsh

zplug 'dracula/zsh', as:theme
# zplug 'sbugzu/gruvbox-zsh', as:theme
zplug "plugins/git",   from:oh-my-zsh
zplug "zsh-users/zsh-autosuggestions"
# zplug "marlonrichert/zsh-autocomplete"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "softmoth/zsh-vim-mode"

# ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#ff00ff,bg=cyan,bold,underline"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load --verbose

zshcache_time="$(date +%s%N)"

autoload -Uz add-zsh-hook

rehash_precmd() {
  if [[ -a /var/cache/zsh/pacman ]]; then
    local paccache_time="$(date -r /var/cache/zsh/pacman +%s%N)"
    if (( zshcache_time < paccache_time )); then
      rehash
      zshcache_time="$paccache_time"
    fi
  fi
}

add-zsh-hook -Uz precmd rehash_precmd

# omz
alias zshconfig="nvim ~/.zshrc"
alias ohmyzsh="thunar ~/.oh-my-zsh"

alias v='nvim'
alias vi='nvim'

# ls
alias l='ls -lh'
alias ll='ls -lah'
alias la='ls -A'
alias lm='ls -m'
alias lr='ls -R'
alias lg='ls -l --group-directories-first'

gac () {
    git add .
    git commit -m "$1"
}

export PATH=$PATH:$(yarn global bin)
 
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

# global esy apps
export PATH=/home/leo/.esy-apps:$PATH

# fnm
export PATH=/home/leo/.fnm:$PATH
eval "`fnm env`"

# opam configuration
[[ ! -r /home/leo/.opam/opam-init/init.zsh ]] || source /home/leo/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"

#flyctl
export FLYCTL_INSTALL="/home/leo/.fly"
export PATH="$FLYCTL_INSTALL/bin:$PATH"

# ZSH_THEME="robbyrussel"

# add ssh-agent
# eval $(ssh-agent -s) && eval ssh-add
#
powerline-daemon -q
. /usr/share/powerline/bindings/zsh/powerline.zsh

export GPG_TTY=$(tty)

function work() {
  eval $(ssh-agent -s)
  ssh-add 
  ssh -L 3003:127.0.0.1:3003 b
}

export AHREFS_MONOREPO="$HOME/ahrefs/monorepo"

