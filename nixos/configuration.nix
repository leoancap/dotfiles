{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;

  programs.nix-ld.enable = true;

  networking.hostName = "nixos";

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_TIME = "es_ES.UTF-8";
    LC_NUMERIC = "es_ES.UTF-8";
  };

  services.xserver.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  services.udev = {
    extraRules = ''
      SUBSYSTEM=="backlight", GROUP="video", MODE="0664"
    '';
  };

  services.keyd = {
    enable = true;
    keyboards = {
      default = {
        ids = [ "*" ]; 
        settings = {
          main = {
            capslock = "layer(control)"; # you might need to also enclose the key in quotes if it contains non-alphabetical symbols
          };
        };
      };
    };
  };

  services.accounts-daemon.enable = true;
  networking.networkmanager.enable = true;

  services.logrotate.enable = true;

  users.users.leo = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" ];
  };

  users.groups = {
    networkmanager = {};
    lightdm = {};
    uinput = {};
    pipewire = {};
  };

  environment.systemPackages = with pkgs; [
    alacritty
    brave
    vim
    zathura
    zsh
    git
    curl
    wget
    nodejs
    dmenu
    arandr
    xrandr
    brightnessctl
    rofi
    polybar
    qbittorrent
    pamixer
    alsa-utils
    pulseaudio
    mpv
    ranger
    appimage-run
    redshift

    # C 
    stdenv.cc
    fontconfig
    freetype
    harfbuzz
    
  ];
}
