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

setup_keyd() {
    local src="$DOTFILES_DIR/keyd"
    local dest="/etc/keyd"

    if [[ ! -e "$src" ]]; then
        log_error "Source does not exist: $src"
        return 1
    fi

    if [[ ! -d "$dest" ]]; then
        log_info "Creating directory: $dest"
        sudo mkdir -p "$dest"
    fi

    for f in "$src"/*.conf; do
        local fname=$(basename "$f")
        if [[ -e "$dest/$fname" && ! -L "$dest/$fname" ]]; then
            local backup="${dest}/${fname}.backup.$(date +%Y%m%d_%H%M%S)"
            log_warn "Backing up existing $dest/$fname to $backup"
            sudo mv "$dest/$fname" "$backup"
        fi
        log_info "Copying: $dest/$fname"
        sudo cp "$f" "$dest/$fname"
    done

    log_info "Starting keyd service..."
    sudo keyd enable 2>/dev/null || true
    sudo systemctl start keyd 2>/dev/null || sudo keyd 2>/dev/null || true
}

setup_lazygit() {
    local version="0.60.0"
    local arch="linux_x86_64"
    local url="https://github.com/jesseduffield/lazygit/releases/download/v${version}/lazygit_${version}_${arch}.tar.gz"
    local install_dir="$HOME/apps"
    local binary_path="$install_dir/lazygit"

    if [[ -x "$binary_path" ]]; then
        log_info "lazygit already installed at $binary_path"
        return 0
    fi

    log_info "Downloading lazygit v$version..."
    mkdir -p "$install_dir"

    if ! curl -sL "$url" -o /tmp/lazygit.tar.gz; then
        log_error "Failed to download lazygit"
        return 1
    fi

    tar -xzf /tmp/lazygit.tar.gz -C "$install_dir" lazygit
    rm /tmp/lazygit.tar.gz

    if [[ -x "$binary_path" ]]; then
        log_info "lazygit installed to $binary_path"
    else
        log_error "Failed to install lazygit"
        return 1
    fi
}

setup_nvim() {
    local url="https://github.com/neovim/neovim/releases/download/nightly/nvim-linux-x86_64.tar.gz"
    local install_dir="$HOME/apps/nvim"
    local binary_path="$install_dir/bin/nvim"

    if [[ -x "$binary_path" ]]; then
        log_info "nvim already installed at $binary_path"
        return 0
    fi

    log_info "Downloading neovim nightly..."
    mkdir -p "$install_dir"

    if ! curl -sL "$url" -o /tmp/nvim.tar.gz; then
        log_error "Failed to download neovim"
        return 1
    fi

    tar -xzf /tmp/nvim.tar.gz -C "$install_dir" --strip-components=1
    rm /tmp/nvim.tar.gz

    if [[ -x "$binary_path" ]]; then
        log_info "nvim installed to $binary_path"
    else
        log_error "Failed to install nvim"
        return 1
    fi
}

setup_xresources() {
    local linked_any=false

    if link_dotfile ".Xresources_light" "$HOME/.Xresources_light"; then
        created+=("$HOME/.Xresources_light")
        linked_any=true
    fi

    if link_dotfile ".Xresources_dark" "$HOME/.Xresources_dark"; then
        created+=("$HOME/.Xresources_dark")
        linked_any=true
    fi

    if link_dotfile ".Xresources" "$HOME/.Xresources"; then
        created+=("$HOME/.Xresources")
        linked_any=true
    fi

    if [[ "$linked_any" == true ]]; then
        log_info "Default Xresources set to light theme (dotfiles/.Xresources)."
    fi
}

echo "========================================"
echo "          Dotfiles Setup"
echo "========================================"
echo ""

if [[ ! -d "$DOTFILES_DIR" ]]; then
    log_error "Dotfiles directory not found: $DOTFILES_DIR"
    exit 1
fi

created=()

prompt_yes_no "Do you want to symlink nvim?" y && link_dotfile "nvim" "$HOME/.config/nvim" && created+=("$HOME/.config/nvim")
prompt_yes_no "Do you want to symlink xmonad?" y && link_dotfile "xmonad" "$HOME/.xmonad" && created+=("$HOME/.xmonad")
prompt_yes_no "Do you want to symlink Xresources (light default, plus dark variant)?" y && setup_xresources
prompt_yes_no "Do you want to symlink tmux?" y && link_dotfile "tmux.conf" "$HOME/.tmux.conf" && created+=("$HOME/.tmux.conf")
prompt_yes_no "Do you want to symlink zsh?" y && link_dotfile "zshrc" "$HOME/.zshrc" && created+=("$HOME/.zshrc")
prompt_yes_no "Do you want to symlink zathura?" y && link_dotfile "zathura" "$HOME/.config/zathura" && created+=("$HOME/.config/zathura")
prompt_yes_no "Do you want to setup keyd?" y && setup_keyd && created+=("keyd")
prompt_yes_no "Do you want to install lazygit?" y && setup_lazygit && created+=("lazygit")
prompt_yes_no "Do you want to install nvim?" y && setup_nvim && created+=("nvim")

echo ""
echo "========================================"
if [[ ${#created[@]} -eq 0 ]]; then
    log_info "No configurations created."
else
    log_info "Done! Configurations created: ${#created[@]}"
    echo ""
    echo "Your symlinks:"
    for f in "${created[@]}"; do
        if [[ "$f" == "keyd" ]]; then
            ls -la /etc/keyd/*.conf 2>/dev/null | grep -E "^-"
        else
            ls -la "$f" 2>/dev/null | grep -E "^l"
        fi
    done
fi
echo "========================================"
