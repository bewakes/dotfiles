import           System.Exit
import           XMonad

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import qualified XMonad.StackSet             as W

import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Ungrab

import           XMonad.Layout.Magnifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ThreeColumns
import           XMonad.Prompt
import qualified XMonad.Prompt.ConfirmPrompt as Cprmt
import           XMonad.Prompt.Man

import           XMonad.Hooks.EwmhDesktops


restartNotify = "notify-send 'Restarting xmonad. Please wait...';"
restartSuccess = "notify-send 'Restarted.';"
restartFail = "notify-send 'Could not Restart xonad';"
compileFail = " 2> /tmp/_xmonad-reompile || (" ++ restartFail
                ++ " termite -e 'nvim /tmp/_xmonad-recompile' &); "
restartCmd = restartNotify
        ++ "if type xmonad; then xmonad --recompile && xmonad --restart && sleep 0.6s " ++ compileFail
        ++ restartSuccess
        ++ "else xmessage xmonad not in \\$PATH: \"$PATH\";"
        ++ restartFail
        ++ "fi"

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , normalBorderColor = "#555555"
    , focusedBorderColor = "#9999bb"
    , layoutHook = smartBorders myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    }
  `additionalKeysP` myKeys
  `removeKeys`
      [ (mod4Mask .|. shiftMask, xK_c)
      , (mod4Mask .|. shiftMask, xK_q)
      ]


myTerminal = "termite"
myKeys =
    [ ("M-S-b", spawn "firefox-developer-edition")
    , ("M-q", kill)
    , ("M-<Return>", spawn myTerminal)
    , ("M-d", spawn "dmenu_run -c -l 20")
    , ("M-c", spawn "xtrlock")
    , ("M-m", manPrompt def)
    , ("M-S-r", spawn restartCmd)
    , ("M-S-C-q", Cprmt.confirmPrompt def "Exit?" $ io (exitWith ExitSuccess))
    , ("M-C-+", sendMessage MagnifyMore)
    , ("M-C--", sendMessage MagnifyLess)
    , ("M-;", namedScratchpadAction myScratchPads "assistant")
    , ("M-n", namedScratchpadAction myScratchPads "notes")
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ] <+>
    namedScratchpadManageHook myScratchPads

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Bottom" "#8be9fd" 1
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . greenish . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . gray    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    greenish, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    greenish = xmobarColor "#7de37f" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    gray     = xmobarColor "#777777" ""
    lowWhite = xmobarColor "#999999" ""


myScratchPads =
    [ NS
        "assistant"
        "termite --class assistant -e assistant"
        (className =? "assistant")
        (customFloating $ W.RationalRect l t w h)
    , NS
        "notes"
        "termite --class notes -e 'nvim /tmp/notes'"
        (className =? "notes")
        (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
    ]
      where
        l = 5/16
        h = 1.2/4
        t = 1 - h
        w = 1.5/4
