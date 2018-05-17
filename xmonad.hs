import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Cursor
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Layout.NoBorders

-- gaps
import XMonad.Layout.Gaps

import qualified XMonad.StackSet as W

-- scratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]

{-myLayoutHook = gaps [(U,10), (R,10), (D, 10), (L, 10)] $ Tall 1 (3/100) (1/2) ||| Full-}

myTerminal = "termite"


main = do
    --xmproc <- spawnPipe "/usr/local/bin/xmobar /home/bibek/.xmobarrc" 
    xmproc <- spawnPipe "/home/bibek/custom_bins/xmobar /home/bibek/.xmobarrc" 
    xmproc1 <- spawnPipe "/home/bibek/custom_bins/xmobar /home/bibek/.xmobaruprc"

    xmonad $ defaultConfig
        { manageHook = composeAll [
                  (isFullscreen --> doFullFloat)
                , manageDocks 
                , myManageHook
                , (scratchpadManageHook $ W.RationalRect l t w h)
                , manageHook defaultConfig
                
        ]
        , layoutHook = avoidStruts  $  smartBorders $ layoutHook defaultConfig
        , startupHook = setDefaultCursor xC_left_ptr
        {-, layoutHook = avoidStruts  $ myLayoutHook-}
        , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
        , terminal = myTerminal
        , borderWidth = 1
        , focusedBorderColor = "skyblue"
        , normalBorderColor = "#777777"
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "skyblue" "" . shorten 50
                        }
        , modMask = mod4Mask

        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((mod4Mask, xK_Return), spawn myTerminal)
        , ((mod4Mask, xK_q), kill)
        , ((mod4Mask, xK_r), spawn "pkill xmobar; xmonad --restart")
        , ((mod4Mask, xK_d), spawn "exec rofi -show run")
        , ((mod4Mask, xK_b), spawn "firefox")
        , ((mod4Mask, xK_l), spawn "xtrlock")
        , ((mod4Mask, xK_w), spawn "feh --randomize --bg-fill ~/Pictures/wallpapers/unsplash/*")
        , ((mod4Mask .|. shiftMask, xK_w), spawn "getwallpaper.sh")
        , ((mod4Mask, xK_h), scratchPad)
        , ((mod4Mask, xK_x), shellPrompt def)

        -- volume control with notification
        , ((mod4Mask .|. shiftMask, xK_i), spawn "amixer -q sset Master 3%+ && volume-notify")
        , ((mod4Mask .|. shiftMask, xK_d), spawn "amixer -q sset Master 3%- && volume-notify")

        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
        where scratchPad = scratchpadSpawnActionCustom "xterm -name scratchpad -e assistant"
              w = 0.4
              h = 0.3
              l = 1-w
              t = 0.7
