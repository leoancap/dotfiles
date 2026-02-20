-- ## Modules ## -------------------------------------------------------------------

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Dwindle
import qualified XMonad.StackSet as W
import XMonad.Util.SpawnOnce

-- ## Startup hook ## ---------------------------------------------------------------
myStartupHook = do
  spawn "bash ~/.xmonad/bin/autostart.sh"

-- ## Applications ## ---------------------------------------------------------------
-- Terminal
myTerminal = "st"

-- Apps
file_manager = spawn "pcmanfm"
web_browser = spawn "brave-browser"
terminal_light= spawn "xrdb merge ~/.Xresources_light && kill -USR1 $(pidof st)"
terminal_dark= spawn "xrdb merge ~/.Xresources && kill -USR1 $(pidof st)"

-- Monitors 
single_monitor = spawn "~/.screenlayout/single-monitor.sh"
dual_monitor = spawn "~/.screenlayout/dual-monitor.sh"
single_eink = spawn "~/.screenlayout/mono-eink.sh"
dual_eink = spawn "~/.screenlayout/dual-eink.sh"
booxPro = spawn "~/.screenlayout/booxPro.sh"
dual_boox = spawn "~/.screenlayout/dual-booxPro.sh"
monitor_only = spawn "~/.screenlayout/monitor-only.sh"

-- Rofi Menus
dmenu = spawn "dmenu_run -l 10 -fn 'monospace:size=16' -nb '#ffffff' -nf '#000000' -sb '#000000' -sf '#ffffff'"
rofi_launcher = spawn "~/.xmonad/rofi/bin/launcher"
rofi_network = spawn "~/.xmonad/rofi/bin/network"
rofi_powermenu = spawn "~/.xmonad/rofi/bin/powermenu"
-- rofi_screenshot = spawn "QT_AUTO_SCREEN_SCALE_FACTOR=0 QT_SCREEN_SCALE_FACTORS=1 QT_SCALE_FACTOR=1 QT_ENABLE_HIGHDPI_SCALING=0 flameshot gui"
rofi_screenshot = spawn "flameshot gui"

-- ## Settings ## -------------------------------------------------------------------
myFocusFollowsMouse = True
myClickJustFocuses = False
myBorderWidth = 2
myFocusedBorderColor = "#BB553F"
myNormalBorderColor = "#E6DFE0"
-- mod1Mask : left alt Key
-- mod4Mask : Windows or Super Key
myModMask = mod4Mask
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

-- ## Key Bindings ## -------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = super}) =
  M.fromList $
    -- launch terminal
    -- [ ((super, xK_Return), 			spawn $ XMonad.terminal conf)
    [ ((super, xK_Return), spawn $ "st"),
      ((super .|. shiftMask, xK_Return), spawn "~/.xmonad/bin/xmoterm.sh -f"),
      -- launch applications
      ((super .|. controlMask, xK_p), file_manager),
      ((super .|. controlMask, xK_f), web_browser),
      ((super .|. controlMask, xK_1), single_monitor),
      ((super .|. controlMask, xK_3), dual_monitor),
      ((super .|. controlMask, xK_4), single_eink),
      ((super .|. controlMask, xK_5), dual_eink),
      ((super .|. controlMask, xK_6), booxPro),
      ((super .|. controlMask, xK_7), dual_boox),
      ((super .|. controlMask, xK_8), monitor_only),
      -- launch rofi menus
      ((mod1Mask, xK_F1), dmenu),
      ((super, xK_d), rofi_launcher),
      ((super, xK_semicolon), dmenu),
      ((super, xK_x), rofi_powermenu),
      ((mod1Mask .|. controlMask, xK_n), rofi_network),
      ((mod1Mask .|. controlMask, xK_s), rofi_screenshot),
      -- Audio keys
      ((0, xF86XK_AudioRaiseVolume), spawn "volume --inc"),
      ((super, xK_equal), spawn "volume --inc"),
      ((0, xF86XK_AudioLowerVolume), spawn "volume --dec"),
      ((super, xK_minus), spawn "volume --dec"),
      ((0, xF86XK_AudioMute), spawn "volume --toggle"),
      ((super, xK_backslash), spawn "volume --toggle"),
     -- Redshift redlight keys
      ((super .|. controlMask, xK_comma), spawn "redshift -P -O 3000"),
      ((super .|. controlMask, xK_period), spawn "redshift -x"),
      -- Brightness keys
      ((0, xF86XK_MonBrightnessUp), spawn "brightness --inc"),
      ((0, xF86XK_MonBrightnessDown), spawn "brightness --dec"),
      -- Screenshot
      ((mod1Mask, xK_Print), spawn $ "takeshot --in5"),
      ((shiftMask, xK_Print), spawn $ "takeshot --in10"),
      ((controlMask, xK_Print), spawn $ "takeshot --win"),
      ((mod1Mask .|. controlMask, xK_Print), spawn $ "takeshot --area"),
      -- Close focused window
      ((super, xK_w), kill),
      -- Note: Super+C/V handled by keyd per-app (see /etc/keyd/default.conf and app.conf)
      -- light theme
      ((super .|. shiftMask, xK_e), terminal_light),
      -- dark theme
      ((super .|. shiftMask, xK_d), terminal_dark),
      -- Window Manager Specific -----------------------------------------
      -- Resize viewed windows to the correct size
      ((super, xK_r), refresh),
      -- Push window back into tiling
      ((super, xK_t), withFocused $ windows . W.sink),
      -- Rotate through the available layout algorithms
      ((super, xK_space), sendMessage NextLayout),
      --  Reset the layouts on the current workspace to default
      ((super .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Move focus to the next window
      ((super, xK_Tab), windows W.focusDown),
      ((super, xK_p), windows W.focusDown),
      ((super, xK_n), windows W.focusUp),
      -- Swap windows
      ((super .|. shiftMask, xK_p), windows W.swapDown),
      ((super .|. shiftMask, xK_n), windows W.swapUp),
      -- Resize windows
      ((super .|. controlMask, xK_n), sendMessage Shrink),
      ((super .|. controlMask, xK_p), sendMessage Expand),
      -- Restart xmonad
      ((super, xK_q), spawn "xmonad --recompile; xmonad --restart")
    ]
      ++
      -- Workspace Specific ---------------------------------------------------------------

      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      [ ((m .|. super, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      -- mod-{u,i,o}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{u,i,o}, Move client to screen 1, 2, or 3
      [ ((m .|. super, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_i, xK_u] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

-- ## Mouse Bindings ## ------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = super}) =
  M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (super, button1),
        ( \w ->
            focus w
              >> mouseMoveWindow w
              >> windows W.shiftMaster
        )
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((super, button2), (\w -> focus w >> windows W.shiftMaster)),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (super, button3),
        ( \w ->
            focus w
              >> mouseResizeWindow w
              >> windows W.shiftMaster
        )
      )
    ]

myLayout = myTiled ||| Mirror tiled ||| myColumn ||| myFull
  where
    withGaps = gaps [(U, 30), (D, 5), (R, 5), (L, 5)]
    tiled = Tall nmaster delta tiled_ratio
    column = ThreeColMid nmaster delta tiled_ratio
    nmaster = 1
    delta = 3 / 100
    tiled_ratio = 1 / 2

    myTiled = withGaps $ smartBorders tiled
    myColumn = withGaps $ smartBorders column
    myFull = noBorders Full

-- ## Layouts ## -------------------------------------------------------------------------
-- myLayout = avoidStruts(tiled ||| Mirror tiled ||| noBorders Full)
-- 	where
-- 		-- default tiling algorithm partitions the screen into two panes
-- 		tiled   = Tall nmaster delta ratio
-- 		-- The default number of windows in the master pane
-- 		nmaster = 1
-- 		-- Default proportion of screen occupied by master pane
-- 		ratio   = 1/2
-- 		-- Percent of screen to increment by when resizing panes
-- 		delta   = 3/100

-- ## Window rules ## --------------------------------------------------------------------
myManageHook =
  composeAll . concat $
    [ [isDialog --> doCenterFloat],
      [className =? c --> doCenterFloat | c <- myCFloats],
      [title =? t --> doCenterFloat | t <- myTFloats],
      [resource =? r --> doFloat | r <- myRFloats],
      [resource =? i --> doIgnore | i <- myIgnores]
    ]
  where
    myCFloats =
      [ "alacritty-float",
        "MPlayer",
        "mpv",
        "Gimp",
        "feh",
        "Viewnior",
        "Gpicview",
        "Kvantum Manager",
        "qt5ct",
        "VirtualBox Manager",
        "qt-menu",
        "qemu",
        "Qemu-system-x86_64",
        "Lxappearance",
        "Nitrogen",
        "Arandr",
        "Pavucontrol",
        "Xfce4-power-manager-settings",
        "Nm-connection-editor"
      ]
    myTFloats = ["Downloads", "Save As...", "About : Aditya Shakya", "Getting Started"]
    myRFloats = []
    myIgnores = ["desktop_window"]

-- ## Event handling ## -------------------------------------------------------------------
-- myEventHook = ewmhDesktopsEventHook

-- ## Logging ## --------------------------------------------------------------------------
myLogHook = spawn "polybar-msg action '#tmux.module_update'"

-- ## Main Function ## --------------------------------------------------------------------

-- Run xmonad with all the configs we set up.
main = xmonad $ fullscreenSupport $ docks $ ewmh defaults

defaults =
  def
    { -- configs
      terminal = myTerminal,
      focusFollowsMouse = myFocusFollowsMouse,
      clickJustFocuses = myClickJustFocuses,
      borderWidth = myBorderWidth,
      modMask = myModMask,
      workspaces = myWorkspaces,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      -- key bindings
      keys = myKeys,
      mouseBindings = myMouseBindings,
      -- hooks, layouts
      -- layoutHook = gaps [(L,0), (R,0), (U,0), (D,0)] $ spacingRaw False (Border 5 0 10 0) True (Border 0 5 0 5) True $ myLayout,
      manageHook = myManageHook,
      layoutHook = myLayout,
      -- handleEventHook = myEventHook,
      logHook = myLogHook,
      startupHook = myStartupHook
    }
