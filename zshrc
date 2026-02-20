source ~/.zplug/init.zsh

zplug 'dracula/zsh', as:theme
zplug "plugins/git",   from:oh-my-zsh
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "softmoth/zsh-vim-mode"

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

alias v= 'vim'
alias vi='vim'
# alias nvim='~/apps/nvim.appimage'
alias nvim='~/apps/nvim/bin/nvim'
alias mira='sudo node ~/apps/mira-js/bin/cli.js'


# ls
alias l='ls -lh'
alias ll='ls -lah'
alias la='ls -A'
alias lm='ls -m'
alias lr='ls -R'
alias lg='ls -l --group-directories-first'

# mira monitor 
alias mira-speed='mira settings --refresh-mode a2 --contrast 8 --speed 7 --dither-mode 0 --white-filter 0 --black-filter 0'
alias mira-text='mira settings --refresh-mode a2 --contrast 7 --speed 6 --dither-mode 1 --white-filter 0 --black-filter 0'
                                                                                                                           
alias mira-text2='mira settings --refresh-mode direct --contrast 1 --speed 7 --dither-mode 0 --white-filter 50 --black-filter 50'
alias mira-text3='mira settings --refresh-mode a2 --contrast 0 --speed 7 --dither-mode 1 --white-filter 110 --black-filter 0'

alias mira-code='mira settings --refresh-mode a2 --contrast 11 --speed 6 --dither-mode 0 --white-filter 3 --black-filter 0'

alias mira-gray='mira settings --refresh-mode a2 --contrast 0 --speed 7 --dither-mode 0 --white-filter 250 --black-filter 250'

alias mira-image='mira settings --refresh-mode direct --contrast 7 --speed 5 --dither-mode 0 --white-filter 0 --black-filter 0'
alias mira-video='mira settings --refresh-mode a2 --contrast 7 --speed 6 --dither-mode 2 --white-filter 10 --black-filter 0'
alias mira-read='mira settings --refresh-mode direct --contrast 7 --speed 5 --dither-mode 3 --white-filter 12 --black-filter 10'
alias mira-warm='mira settings --warm-light'

# iris flower
alias block-blue='iris-flower 80 100 30 10'
alias allow-blue='iris-flower 80 100 100 100'

# theme switcher
alias terminal_light="xrdb merge ~/.Xresources_light && kill -USR1 $(pidof st)"
alias terminal_dark="xrdb merge ~/.Xresources && kill -USR1 $(pidof st)"

gac () {
    git add .
    git commit -m "$1"
}

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

# global pip apps
export PATH=/home/leo/.local/bin/:$PATH

# opam configuration
[[ ! -r /home/leo/.opam/opam-init/init.zsh ]] || source /home/leo/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"

#flyctl
export GPG_TTY=$(tty)

# add eink-monitor mode
function add-mode-eink() {
  xrandr --newmode "1680x1292_60.00"  110.75  1680 1776 1944 2208  1292 1295 1305 1323 -hsync +vsync\
  xrandr --addmode DisplayPort-1 "1680x1292_60.00"
}

# add eink-monitor mode
function add-mode-eink-small() {
  xrandr --newmode  "1264x969_60.00"  101.25  1264 1344 1472 1680  969 972 982 1006 -hsync +vsync
  xrandr --addmode DisplayPort-1 "1264x969_60.00"
}

function add-mode-eink-xsmall() {
  xrandr --newmode "904x693_60.00"  50.00  904 944 1032 1160  693 696 706 720 -hsync +vsync
  xrandr --addmode DisplayPort-1 "904x693_60.00"
}


# SSH config
# Oh-my-zsh compatible bash ssh-agent start script
eval $(keychain --eval --quiet)

#

# Ahrefs config
export AHREFS_MONOREPO="$HOME/ahrefs/monorepo"
# Load secrets (API keys, etc.) - not tracked in git
[[ -f ~/.secrets ]] && source ~/.secrets

export PATH="$HOME/.bin:$PATH"

export EDITOR=nvim

# stty erase ^H


# opencode
export PATH=/home/leo/.opencode/bin:$PATH
export PATH="$HOME/.npm-global/bin:$PATH"
