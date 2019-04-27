--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import qualified XMonad.StackSet as W 
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO


-- utils
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Prompt.Shell
import XMonad.Prompt


-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "termite"

xmonadPath =  "~/home/bibek/.xmonad/xmonad-x86_64-linux"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "skyblue"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

-- assistantScratchPad = scratchpadSpawnActionCustom  "termite -t assistant -e assistant"
assistantScratchPad = scratchpadSpawnActionCustom "xterm -name scratchpad -e assistant"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm, xK_Return), spawn myTerminal)

    -- launch dmenu/rofi
    , ((modm, xK_d), spawn "exec rofi -show run")

    -- launch gmrun
    -- , ((modm .|. shiftMask, xK_p), spawn "gmrun")

    -- close focused window
    , ((modm, xK_q), kill)

    -- launch firefox
    , ((modm, xK_b), spawn "firefox")

     -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)

    -- Lock the screen
    , ((modm, xK_c), spawn "xtrlock")

    -- Change the wallpaper in random
    , ((modm, xK_w), spawn "feh --randomize --bg-fill ~/Pictures/wallpapers/unsplash/*")

    -- Download wallpaper(custom script)
    , ((modm .|. shiftMask, xK_w), spawn "getwallpaper.sh")

    -- launch assistant scratchpad
    , ((modm, xK_semicolon), assistantScratchPad)

    -- change screen layouts(NOTE: needs to be better)
    , ((modm .|. shiftMask, xK_t), spawn "sh /home/bibek/.screenlayout/toggle.sh")
    , ((modm .|. shiftMask, xK_n), spawn "sh /home/bibek/.screenlayout/normal.sh")

    -- volume control with notification
    , ((modm .|. shiftMask, xK_i), spawn "amixer -q sset Master 3%+ && volume-notify")
    , ((modm .|. shiftMask, xK_d), spawn "amixer -q sset Master 3%- && volume-notify")

    -- DON'T KNOW ABOUT THIS, probably screen capture
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")

    -- Move focus to the next window
    , ((modm, xK_Tab), windows W.focusDown)

    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm, xK_m), windows W.focusMaster  )

    -- Swap the focused window and the master window
    -- , ((modm, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp    )

    -- Shrink the master area
    --, ((modm, xK_h), sendMessage Shrink)

    -- Expand the master area
    --, ((modm, xK_l), sendMessage Expand)
    , ((modm, xK_h ), sendMessage Shrink)
    , ((modm, xK_l ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_h ), sendMessage MirrorShrink)
    , ((modm .|. shiftMask, xK_l ), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r), spawn $ "xmonad --recompile; xmonad --restart")

    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e}, Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{w,e,r}, Move client to screen 1 or 2
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--


--- My Theme For Tabbed layout
myTheme = defaultTheme { decoHeight = 16
                        , activeColor = "#a6c292"
                        , activeBorderColor = "#a6c292"
                        , activeTextColor = "#000000"
                        , inactiveBorderColor = "#000000"
                        }


--LayoutHook
myLayoutHook = standardLayouts 
   where
    standardLayouts =   avoidStruts $ smartBorders $ (tiled |||  reflectTiled ||| Mirror tiled ||| Grid ||| Full) 

    --Layouts
    tiled     = smartBorders (ResizableTall 1 (2/100) (1/2) [])
    reflectTiled = (reflectHoriz tiled)
    tabLayout = (tabbed shrinkText myTheme)
    full      = noBorders Full

    --Web Layout
    webL      = avoidStruts $  tabLayout  ||| tiled ||| reflectHoriz tiled |||  full 

    --VirtualLayout
    fullL = avoidStruts $ full


------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myScratchPads = [
      NS "assistant" spawnAssistant findAssistant manageAssistant
    --, NS "scratch" spawnScratch findScratch manageScratch
    ]
    where spawnAssistant = "termite -title assistant -e assistant"
          findAssistant = className =? "assistant"
          manageAssistant = customFloating $ W.RationalRect l t w h
            where ih = 0.6       -- height, 60% 
                  iw = 0.6       -- width, 60% 
                  it = (1 - h)/2 -- centered top/bottom
                  il = (1 - w)/2 -- centered left/right
                  w = 0.4
                  h = 0.3
                  l = 1-w
                  t = 0.7

myManageHook = composeAll [
      (isFullscreen --> doFullFloat)
    , manageDocks
    , manageScratchPad
    , manageHook defaultConfig
    ] <+> namedScratchpadManageHook myScratchPads

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    w = 0.4
    h = 0.3
    l = 1-w
    t = 0.68
------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = handleEventHook defaultConfig <+> docksEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn h }


 

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc <- spawnPipe "xmobar /home/bibek/.xmobarrc"
    xmonad $ defaults xmproc

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults xmproc = defaultConfig {
      -- simple stuff
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
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook xmproc,
        startupHook        = myStartupHook
    }
