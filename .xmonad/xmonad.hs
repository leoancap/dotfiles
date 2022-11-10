-- Copyright (C) 2020-2021 Aditya Shakya <adi1090x@gmail.com>
-- Everyone is permitted to copy and distribute copies of this file under GNU-GPL3
--
-- Xmonad config for Archcraft 

-- ## Modules ## -------------------------------------------------------------------
import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Gaps

import XMonad.Actions.CycleWS

import Graphics.X11.ExtraTypes.XF86
import System.Exit
import Control.Monad
import Data.Monoid
import Data.Maybe

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- ## Startup hook ## ---------------------------------------------------------------
myStartupHook = do
	spawn "bash ~/.xmonad/bin/autostart.sh"

-- ## Applications ## ---------------------------------------------------------------
-- Terminal
myTerminal      	= "~/.xmonad/bin/xmoterm.sh"

-- Apps
file_manager		        = spawn "pcmanfm"
web_browser			= spawn "librewolf"

-- Rofi Menus
single_monitor 		= spawn "~/.screenlayout/single-mon.sh"
tv_monitor 		= spawn "~/.screenlayout/dual-mon-desk.sh"
invertMonitor1 		= spawn "xrandr-invert-colors -s 0"
invertMonitor2 		= spawn "xrandr-invert-colors -s 1"

-- Rofi Menus
rofi_asroot 		= spawn "~/.xmonad/rofi/bin/asroot"
rofi_launcher 		= spawn "~/.xmonad/rofi/bin/launcher"
rofi_mpd 			= spawn "~/.xmonad/rofi/bin/mpd"
rofi_network 		= spawn "~/.xmonad/rofi/bin/network"
rofi_network_menu 	= spawn "~/.xmonad/rofi/bin/network_menu"
rofi_powermenu 		= spawn "~/.xmonad/rofi/bin/powermenu"
rofi_screenshot 	= spawn "flameshot gui"
rofi_windows 		= spawn "~/.xmonad/rofi/bin/windows"

-- ## Settings ## -------------------------------------------------------------------

-- focus follows the mouse pointer
myFocusFollowsMouse 	:: Bool
myFocusFollowsMouse 	= True

-- clicking on a window to focus
myClickJustFocuses 		:: Bool
myClickJustFocuses 		= False

-- Width of the window border in pixels
myBorderWidth   		= 4

-- Border colors for focused & unfocused windows
myFocusedBorderColor 	= "#BB553F"
myNormalBorderColor  	= "#E6DFE0"

-- modMask : modkey you want to use
-- mod1Mask : left alt Key
-- mod4Mask : Windows or Super Key
myModMask       		= mod4Mask

-- Workspaces (ewmh)
myWorkspaces    		= ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

-- ## Key Bindings ## -------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = super}) = M.fromList $

    -- launch terminal
    -- [ ((super, xK_Return), 			spawn $ XMonad.terminal conf)
    [ ((super, xK_Return), 			spawn $ "st")
    , ((super .|. shiftMask, 		xK_Return), spawn "~/.xmonad/bin/xmoterm.sh -f")

	-- launch applications
    , ((super .|. controlMask, 		        xK_p), 		file_manager)
    , ((super .|. controlMask, 		        xK_f), 		web_browser)
    , ((super .|. controlMask, 		        xK_1),          single_monitor) 
    , ((super .|. controlMask, 		        xK_3),          tv_monitor)
    , ((mod1Mask .|. controlMask, 		xK_1),          invertMonitor1) 
    , ((mod1Mask .|. controlMask, 		xK_3),          invertMonitor2)

    -- launch rofi menus
    , ((mod1Mask,           		xK_F1), 	rofi_launcher)
    , ((super,           		xK_p), 	        rofi_launcher)
    , ((super,               		xK_n), 		rofi_network_menu)
    , ((super,               		xK_x), 		rofi_powermenu)
    , ((mod1Mask .|. controlMask, 	xK_m), 		rofi_mpd)
    , ((mod1Mask .|. controlMask, 	xK_n), 		rofi_network)
    , ((mod1Mask .|. controlMask, 	xK_s), 		rofi_screenshot)
    , ((mod1Mask .|. controlMask, 	xK_r), 		rofi_asroot)
    , ((mod1Mask .|. controlMask, 	xK_w), 		rofi_windows)

    -- Audio keys
    , ((0,         xF86XK_AudioPlay), 			spawn "mpc toggle")
    , ((0,         xF86XK_AudioPrev), 			spawn "mpc prev")
    , ((0,         xF86XK_AudioNext), 			spawn "mpc next")
    , ((0,         xF86XK_AudioStop), 			spawn "mpc stop")
    , ((0,         xF86XK_AudioRaiseVolume), 	spawn "volume --inc")
    , ((0,         xF86XK_AudioLowerVolume), 	spawn "volume --dec")
    , ((0,         xF86XK_AudioMute), 			spawn "volume --toggle")

    -- Brightness keys
    , ((0,         xF86XK_MonBrightnessUp), 	spawn "brightness --inc")
    , ((0,         xF86XK_MonBrightnessDown), 	spawn "brightness --dec") 

    -- Screenshot
    , ((0, 							xK_Print), 	spawn $ "takeshot --now")
    , ((mod1Mask, 					xK_Print), 	spawn $ "takeshot --in5")
    , ((shiftMask, 					xK_Print), 	spawn $ "takeshot --in10")
    , ((controlMask,				xK_Print), 	spawn $ "takeshot --win")
    , ((mod1Mask .|. controlMask  , xK_Print), 	spawn $ "takeshot --area")

    -- Close focused window
    , ((super, 		xK_w), 						kill)
    , ((super, 		xK_c), 						kill)
    , ((super, 		xK_Escape), 				spawn "xkill")
    
    -- Lockscreen
    , ((mod1Mask .|. controlMask, 	xK_l), 		spawn "~/.xmonad/bin/xmolock.sh")

    -- Change gaps on the fly
    , ((mod1Mask .|. controlMask, 	xK_g), sendMessage $ ToggleGaps)               					-- toggle all gaps
    , ((super .|. shiftMask, 	xK_g), sendMessage $ setGaps [(L,5), (R,5), (U,35), (D,5)]) 	-- reset the GapSpec
    
    , ((super .|. controlMask, 	xK_t), sendMessage $ IncGap 5 L)     -- increment the left-hand gap
    , ((super .|. shiftMask, 	xK_t), sendMessage $ DecGap 5 L)     -- decrement the left-hand gap
    
    , ((super .|. controlMask, 	xK_y), sendMessage $ IncGap 5 U)     -- increment the top gap
    , ((super .|. shiftMask, 	xK_y), sendMessage $ DecGap 5 U)     -- decrement the top gap
    
    , ((super .|. controlMask, 	xK_r), sendMessage $ IncGap 5 D)     -- increment the bottom gap
    , ((super .|. shiftMask, 	xK_r), sendMessage $ DecGap 5 D)     -- decrement the bottom gap

    , ((super .|. controlMask, 	xK_e), sendMessage $ IncGap 5 R)     -- increment the right-hand gap
    , ((super .|. shiftMask, 	xK_e), sendMessage $ DecGap 5 R)     -- decrement the right-hand gap

	-- Window Manager Specific -----------------------------------------

    -- Resize viewed windows to the correct size
    , ((super,                   xK_r), 		refresh)

    -- Move focus to the master window
    , ((super,               	xK_m), 		windows W.focusMaster)

    -- Swap the focused window and the master window
    , ((super,               	xK_s), 		windows W.swapMaster)

    -- Push window back into tiling
    , ((super,               	xK_t),		withFocused $ windows . W.sink)

    -- Rotate through the available layout algorithms
    , ((super,               xK_space), 		sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((super .|. shiftMask, xK_space), 		setLayout $ XMonad.layoutHook conf)

    -- Move focus to the next window
    , ((super,                 xK_Tab), 		windows W.focusDown)

    -- Move focus to the next window
    , ((super,                xK_h),    		windows W.focusDown)
    , ((super,                xK_Left), 		windows W.focusDown)

    -- Move focus to the previous window
    , ((super,               xK_l),      		windows W.focusUp)
    , ((super,               xK_Right), 		windows W.focusUp)

    -- Move workspaces
    , ((super,          	xK_j),		prevWS)
    , ((super,           	xK_k),		nextWS)

    -- Swap the focused window with the next window
    , ((super .|. shiftMask, 	xK_j),		windows W.swapDown)
    , ((super .|. shiftMask, 	xK_Left),	windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((super .|. shiftMask, 	xK_k),		windows W.swapUp)
    , ((super .|. shiftMask, 	xK_Right),	windows W.swapUp)

    -- Shrink the master area
    , ((super .|. shiftMask,     xK_h),		sendMessage Shrink)
    , ((super .|. controlMask,   xK_Left),	sendMessage Shrink)

    -- Expand the master area
    , ((super .|. shiftMask,     xK_l),		sendMessage Expand)
    , ((super .|. controlMask,   xK_Right),	sendMessage Expand)

    -- Increment the number of windows in the master area
    , ((super, 					xK_comma),		sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((super,					xK_period),		sendMessage (IncMasterN (-1)))

    -- Restart xmonad
    , ((super, 						 xK_q),		spawn "xmonad --recompile; xmonad --restart")

    ]
    ++

	-- Workspace Specific ---------------------------------------------------------------

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. super, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{u,i,o}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{u,i,o}, Move client to screen 1, 2, or 3
    [((m .|. super, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_u, xK_i] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- ## Mouse Bindings ## ------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = super}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((super, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((super, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((super, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myLayout = myTiled ||| myTiled2 ||| myColumn ||| myFull
  where
    withGaps = gaps [(U, 35), (D, 5), (R, 5), (L, 5)]
    tiled = Tall nmaster delta tiled_ratio
    tiled2 = Tall 1 (3/100) (1/2)
    column = ThreeColMid nmaster delta tiled_ratio
    nmaster = 1
    delta = 3 / 100
    tiled_ratio = 1 / 2

    myTiled = withGaps $ smartBorders tiled
    myTiled2 = withGaps $ smartBorders tiled2
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
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doCenterFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    ]
    where
		myCFloats = ["alacritty-float", "MPlayer", "mpv",
					"Gimp", "feh", "Viewnior", "Gpicview",
					"Kvantum Manager", "qt5ct", "VirtualBox Manager", "qemu", "Qemu-system-x86_64",
					"Lxappearance", "Nitrogen", "Arandr", "Pavucontrol", "Xfce4-power-manager-settings", "Nm-connection-editor"]
		myTFloats = ["Downloads", "Save As...", "About : Aditya Shakya", "Getting Started"]
		myRFloats = []
		myIgnores = ["desktop_window"]

-- ## Event handling ## -------------------------------------------------------------------
myEventHook = ewmhDesktopsEventHook

-- ## Logging ## --------------------------------------------------------------------------
myLogHook = return ()

-- ## Main Function ## --------------------------------------------------------------------

-- Run xmonad with all the configs we set up.
main = xmonad $ fullscreenSupport $ docks $ ewmh defaults

defaults = def {
      -- configs
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        -- layoutHook = gaps [(L,0), (R,0), (U,0), (D,0)] $ spacingRaw False (Border 5 0 10 0) True (Border 0 5 0 5) True $ myLayout,
	manageHook = myManageHook,
	layoutHook = myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
