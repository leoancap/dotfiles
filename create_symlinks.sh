#!/bin/bash

DOTFILES_DIR="$HOME/dotfiles"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

prompt_yes_no() {
    local prompt="$1"
    local default="${2:-y}"
    local yn
    if [[ "$default" == "y" ]]; then
        read -p "$prompt [Y/n] " yn
        [[ "$yn" =~ ^[Nn]$ ]] && return 1
    else
        read -p "$prompt [y/N] " yn
        [[ "$yn" =~ ^[Yy]$ ]] || return 1
    fi
    return 0
}

link_dotfile() {
    local src="$DOTFILES_DIR/$1"
    local dest="$2"
    local dest_dir=$(dirname "$dest")

    if [[ ! -e "$src" ]]; then
        log_error "Source does not exist: $src"
        return 1
    fi

    if [[ ! -d "$dest_dir" ]]; then
        log_info "Creating directory: $dest_dir"
        mkdir -p "$dest_dir"
    fi

    if [[ -e "$dest" && ! -L "$dest" ]]; then
        local backup="${dest}.backup.$(date +%Y%m%d_%H%M%S)"
        log_warn "Backing up existing $dest to $backup"
        mv "$dest" "$backup"
    fi

    if [[ -L "$dest" ]]; then
        log_info "Removing existing symlink: $dest"
        rm "$dest"
    fi

    log_info "Linking: $dest -> $src"
    ln -s "$src" "$dest"
}

echo "========================================"
echo "       Dotfiles Symlink Setup"
echo "========================================"
echo ""

if [[ ! -d "$DOTFILES_DIR" ]]; then
    log_error "Dotfiles directory not found: $DOTFILES_DIR"
    exit 1
fi

created=()

prompt_yes_no "Do you want to symlink nvim?" y && link_dotfile "nvim" "$HOME/.config/nvim" && created+=("$HOME/.config/nvim")
prompt_yes_no "Do you want to symlink xmonad?" y && link_dotfile "xmonad" "$HOME/.xmonad" && created+=("$HOME/.xmonad")
prompt_yes_no "Do you want to symlink st?" y && link_dotfile "st" "$HOME/.st" && created+=("$HOME/.st")
prompt_yes_no "Do you want to symlink tmux?" y && link_dotfile "tmux.conf" "$HOME/.tmux.conf" && created+=("$HOME/.tmux.conf")
prompt_yes_no "Do you want to symlink zsh?" y && link_dotfile "zshrc" "$HOME/.zshrc" && created+=("$HOME/.zshrc")

echo ""
echo "========================================"
if [[ ${#created[@]} -eq 0 ]]; then
    log_info "No symlinks created."
else
    log_info "Done! Symlinks created: ${#created[@]}"
    echo ""
    echo "Your symlinks:"
    for f in "${created[@]}"; do
        ls -la "$f" 2>/dev/null | grep -E "^l"
    done
fi
echo "========================================"
