#!/bin/bash

# Dotfiles symlink setup script
# Run this on a new machine after cloning the dotfiles repo

DOTFILES_DIR="$HOME/dotfiles"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Backup and link function
# Usage: link_dotfile <source_in_dotfiles> <target_location>
link_dotfile() {
    local src="$DOTFILES_DIR/$1"
    local dest="$2"
    local dest_dir=$(dirname "$dest")

    # Check if source exists
    if [[ ! -e "$src" ]]; then
        log_error "Source does not exist: $src"
        return 1
    fi

    # Create parent directory if needed
    if [[ ! -d "$dest_dir" ]]; then
        log_info "Creating directory: $dest_dir"
        mkdir -p "$dest_dir"
    fi

    # If destination exists and is not a symlink, back it up
    if [[ -e "$dest" && ! -L "$dest" ]]; then
        local backup="${dest}.backup.$(date +%Y%m%d_%H%M%S)"
        log_warn "Backing up existing $dest to $backup"
        mv "$dest" "$backup"
    fi

    # If destination is already a symlink, remove it
    if [[ -L "$dest" ]]; then
        log_info "Removing existing symlink: $dest"
        rm "$dest"
    fi

    # Create the symlink
    log_info "Linking: $dest -> $src"
    ln -s "$src" "$dest"
}

echo "========================================"
echo "       Dotfiles Symlink Setup"
echo "========================================"
echo ""

# Ensure dotfiles directory exists
if [[ ! -d "$DOTFILES_DIR" ]]; then
    log_error "Dotfiles directory not found: $DOTFILES_DIR"
    log_error "Clone your dotfiles repo first:"
    log_error "  git clone <your-repo> ~/dotfiles"
    exit 1
fi

# Create symlinks
link_dotfile "nvim"       "$HOME/.config/nvim"
link_dotfile "xmonad"     "$HOME/.xmonad"
link_dotfile "tmux.conf"  "$HOME/.tmux.conf"
link_dotfile "zshrc"      "$HOME/.zshrc"

echo ""
echo "========================================"
log_info "Done! Symlinks created."
echo "========================================"
echo ""
echo "Your symlinks:"
ls -la ~/.config/nvim ~/.xmonad ~/.tmux.conf ~/.zshrc 2>/dev/null | grep -E "^l"
