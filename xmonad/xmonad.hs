import System.Exit
import qualified Data.Map as M
import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.DynamicLog
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
--import XMonad.Prompt (defaultXPConfig)
import XMonad.Hooks.ICCCMFocus
import qualified XMonad.StackSet as W
import qualified Data.Map as M
--import Data.Ratio ((%))
import XMonad.Actions.CycleWS
import XMonad.Config.Desktop
 
myFont = "xft:Fira Mono For Powerline:size=16"

-- xmonadPath = "~/home/bibek/.xmonad/xmonad-x86_64-linux"
xmonadPath = "xmonad"
 
myXpConfig :: XPConfig
myXpConfig = def {
    borderColor     = "DarkOrange"
    , bgColor       = "black"
    , fgColor       = "#F46D43" --orange
    , bgHLight      = "#42CBF5" --blue
    , fgHLight      = "#f8f8f8"
    , position      = Bottom
    , font          = myFont
    , height        = 24
    , defaultText   = []
}
 
modM                 = mod4Mask
myFocusedBorderColor = "skyblue"
myNormalBorderColor  = "#cccccc"
myBorderWidth        = 1
myTerminal           = "alacritty"
myTitleColor         = "#eeeeee"
myTitleLength        = 80
myCurrentWSColor     = "orange"
myVisibleWSColor     = "#c185a7"
myUrgentWSColor      = "#cc0000"
myWorkspaces         = ["1", "2", "3", "4","5", "6", "7", "8", "9"]
startupWorkspace     = "1"
normalBrowser = "firefox-developer-edition"
workBrowser = normalBrowser ++ " -P \"bewakes-toggle\""

defaultLayouts       = smartBorders(avoidStruts(
  ResizableTall 1 (3/100) (1/2) []
  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
  ||| noBorders Full
  ||| Grid))

myScratchPads =
  [ NS "assistant" spawnAssistant findAssistant manageAssistant ]
  where
    -- NOTE: Only the following config works, Could not make it work with alacritty
    spawnAssistant = "xterm -e assistant"
    findAssistant = resource =? "xterm"
    manageAssistant = customFloating $ W.RationalRect l t w h
      where
        w = 0.4
        h = 0.3
        l = 1 - w
        t = 0.7

myKeyBindings =
    [
      ((modM, xK_b), sendMessage ToggleStruts)
    , ((modM , xK_Return), spawn myTerminal)
    , ((modM, xK_d), shellPrompt myXpConfig)
    , ((modM,xK_q), kill)
    -- Lock the screen
    , ((modM, xK_c), spawn "xtrlock")
    -- Open browser
    , ((modM .|. shiftMask, xK_b), spawn workBrowser)
    , ((modM .|. shiftMask, xK_f), spawn normalBrowser)
    -- Change the wallpaper in random
    , ((modM, xK_g), spawn "feh --randomize --bg-fill ~/Pictures/wallpapers/generative-stuffs/*")
    -- Download wallpaper(custom script)
    , ((modM .|. shiftMask, xK_g), spawn "getwallpaper.sh")
    -- launch assistant scratchpad
    , ((modM, xK_semicolon), namedScratchpadAction myScratchPads "assistant")

    -- WINDOW RELATED BINDINGS
    , ((modM, xK_Right), sendMessage Shrink)
    -- Toggle FullScreen
    -- , ((modM, xK_f), sendMessage $ MT.Toggle FULL)
    , ((modM, xK_Left), sendMessage Expand)
    , ((modM, xK_Down), sendMessage MirrorShrink)
    , ((modM, xK_Up), sendMessage MirrorExpand)
      -- Push window back into tiling
    , ((modM, xK_t), withFocused $ windows . W.sink)
    --  Reset the layouts on the current workspace to default
    -- , ((modM .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    , ((modM .|. shiftMask, xK_t), spawn "sh /home/bibek/.screenlayout/toggle.sh")
    , ((modM .|. shiftMask, xK_n), spawn "sh /home/bibek/.screenlayout/normal.sh")
    -- volume control with notification
    , ((modM .|. shiftMask, xK_i), spawn "amixer -q sset Master 3%+ && volume-notify")
    , ((modM .|. shiftMask, xK_d), spawn "amixer -q sset Master 3%- && volume-notify")

    -- Quit xmonad
    , ((modM .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modM .|. shiftMask, xK_r), spawn $ xmonadPath ++ " --recompile; " ++ xmonadPath  ++ " --restart")
    ]

myManagementHooks :: [ManageHook]
myManagementHooks = [
  resource =? "synapse" --> doIgnore
  , resource =? "stalonetray" --> doIgnore
  , className =? "rdesktop" --> doFloat
  , (className =? "Komodo IDE") --> doF (W.shift "5:Dev")
  , (className =? "Komodo IDE" <&&> resource =? "Komodo_find2") --> doFloat
  , (className =? "Komodo IDE" <&&> resource =? "Komodo_gotofile") --> doFloat
  , (className =? "Komodo IDE" <&&> resource =? "Toplevel") --> doFloat
  , (className =? "Empathy") --> doF (W.shift "7:Chat")
  , (className =? "Pidgin") --> doF (W.shift "7:Chat")
  , (className =? "Gimp-2.8") --> doF (W.shift "9:Pix")
  ]
 
numPadKeys =
  [
      xK_KP_Home, xK_KP_Up, xK_KP_Page_Up
    , xK_KP_Left, xK_KP_Begin,xK_KP_Right
    , xK_KP_End, xK_KP_Down, xK_KP_Page_Down
    , xK_KP_Insert, xK_KP_Delete, xK_KP_Enter
  ]
 
myKeys = myKeyBindings ++
  [
    ((m .|. modM, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numPadKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

myManageHook =
    composeAll
      [ isFullscreen --> doFullFloat
      , manageDocks
      ] <+>
    namedScratchpadManageHook myScratchPads

main = do
  xmproc <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $ defaultConfig {
    focusedBorderColor = myFocusedBorderColor
  , logHook            = dynamicLogWithPP $ xmobarPP {
          ppOutput         = hPutStrLn xmproc
        , ppCurrent        = xmobarColor myCurrentWSColor "" . wrap "[" "]"
        , ppVisible        = xmobarColor myVisibleWSColor "" . wrap "" ""
        , ppUrgent         = xmobarColor myUrgentWSColor  "" . wrap "{" "}"
        , ppHidden         = xmobarColor "skyblue" ""        . wrap "" ""
        , ppHiddenNoWindows= xmobarColor "#888888" "" }
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor
  , focusFollowsMouse  = False
  -- , terminal           = myTerminal
  , borderWidth        = myBorderWidth
  , layoutHook         = avoidStruts defaultLayouts
  , modMask            = modM
  , handleEventHook    = handleEventHook defaultConfig <+> docksEventHook

  {-
  , startupHook        = do
      setWMName "LG3D"
      windows $ W.greedyView startupWorkspace
  -}
  , manageHook         = myManageHook
  }
    `additionalKeys` myKeys
