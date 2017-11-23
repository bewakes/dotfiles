import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Prompt
import XMonad.Prompt.Shell

import qualified XMonad.StackSet as W

-- scratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
	, className =? "Vncviewer" --> doFloat
	]


main = do
    xmproc <- spawnPipe "/usr/local/bin/xmobar /home/janaki/.xmobarrc"
    xmproc1 <- spawnPipe "/usr/local/bin/xmobar /home/janaki/.xmobaruprc"

    xmonad $ defaultConfig
        { manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
            <+> (scratchpadManageHook $ W.RationalRect l t w h)
                        <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
        , terminal = "gnome-terminal"
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask

        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((mod4Mask, xK_Return), spawn "gnome-terminal")
        , ((mod4Mask, xK_q), kill)
        , ((mod4Mask, xK_r), spawn "xmonad --restart")
        , ((mod4Mask, xK_d), spawn "dmenu_run")
        , ((mod4Mask, xK_b), spawn "firefox")
        , ((mod4Mask, xK_h), scratchPad)
        , ((mod4Mask, xK_x), shellPrompt def)
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
        where scratchPad = scratchpadSpawnActionCustom "xterm -name scratchpad -e 'bash /home/janaki/dotdot/assistant/assistant.sh; bash'"
              w = 0.4
              h = 0.35
              l = 1-w
              t = 0.6
