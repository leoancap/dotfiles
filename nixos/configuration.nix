{ config, pkgs, ... }:

let
  pamIncludeLogin = ''
    auth     include login
    account  include login
    password include login
    session  include login
  '';
in

let
  hubstaff = pkgs.buildFHSEnv {
    name = "hubstaff";
    targetPkgs = pkgs: with pkgs; [
      xorg.libSM
      xorg.libICE
      xorg.libX11
      xorg.libXext
      xorg.libXi
      xorg.libXrender
      xorg.libXtst
      xorg.libXfixes
      xorg.libXcursor
      xorg.libXft
      xorg.libXinerama
      glib
      gtk3
      nss
      nspr
      dbus
      atk
      cups
      libdrm
      pango
      cairo
      alsa-lib
      mesa
      libGL
      zlib
      fontconfig
      freetype
    ];
    runScript = "/home/leo/Hubstaff/HubstaffClient.bin.x86_64";
  };
in

{
  imports = [ /etc/nixos/hardware-configuration.nix ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.grub.configurationLimit = 50; 

  programs.nix-ld.enable = true;

  nixpkgs.config.allowUnfree = true;

  networking.hostName = "nixos";

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_TIME = "es_ES.UTF-8";
    LC_NUMERIC = "es_ES.UTF-8";
  };

  services.xserver.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.sessionCommands = ''
    xset r rate 150 70
  '';
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  services.udev = {
    extraRules = ''
      SUBSYSTEM=="backlight", GROUP="video", MODE="0664"
    '';
  };

  services.keyd = {
    enable = true;
    keyboards.default = {
      ids = [ "*" ];
      settings = {
        main = {
          leftmeta = "leftalt";
          leftalt = "overload(nav, leftmeta)";
          rightalt = "layer(altgr_nav)";
          capslock = "overload(control, esc)";
        };
        "nav:M" = {
          h = "left";
          j = "down";
          k = "up";
          l = "right";
        };
        altgr_nav = {
          h = "left";
          j = "down";
          k = "up";
          l = "right";
        };
      };
    };
    keyboards.hhkb = {
      ids = [ "04fe:0016" ];
      settings = {
        main = {
          leftmeta = "layer(nav)";
          rightmeta = "layer(nav)";
          delete = "backspace";
          backspace = "delete";
          capslock = "overload(control, esc)";
          tab = "overload(nav_layer, tab)";
        };
        "nav:M" = {
          h = "left";
          j = "down";
          k = "up";
          l = "right";
          c = "A-c";
          v = "C-S-v";
        };
      };
    };
  };

  services.accounts-daemon.enable = true;
  networking.networkmanager.enable = true;

  services.logrotate.enable = true;

  security.pam.services = {
    i3lock.text = pamIncludeLogin;
  };

  environment.etc."pam.d/i3lock".text = pamIncludeLogin;

  programs.zsh.enable = true;
  programs.zsh.initialization = ''
    [[ -n $DISPLAY ]] && xset r rate 150 70
  '';

  users.users.leo = {
    isNormalUser = true;
    shell = pkgs.zsh;
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
    tmux
    pcmanfm
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
    wireguard-tools
    flameshot
    i3lock-color
    gh
    steam-run
    hubstaff
    gnumake
    keychain

    # C 
    stdenv.cc
    fontconfig
    freetype
    harfbuzz



    (st.overrideAttrs (oldAttrs: {
      src = ../st;
      buildInputs = (oldAttrs.buildInputs or []) ++ [
        pkgs.harfbuzz
      ];
    }))

    (writeShellScriptBin "brave-work" ''
      exec ${brave}/bin/brave --profile-directory="Work" "$@"
    '')

    (writeShellScriptBin "brave-personal" ''
      exec ${brave}/bin/brave --profile-directory="Personal" "$@"
    '')


  ];
}
