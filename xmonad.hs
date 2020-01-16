import Data.Map
import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe) 
import System.IO

myTerminal = "lxterminal"
myBrowser  = "qutebrowser"
mySreenLockCmd = "xscreensaver-command -lock"
myKeys = 
    [ ("M-b", spawn myBrowser)
    , ("M-w", kill)
    , ("M-f", spawn (myTerminal ++ " -e vifm"))
    , ("M-S-l", spawn mySreenLockCmd)
    , ("M-p", spawn "rofi -show drun")
    ]

mySpacedLayout =
    spacingRaw
        -- Use smart spacing (space only when 2 or more windows are open)
        True 
        -- Screen border
        (Border 0 0 0 0) True 
        -- Window border
        (Border 5 5 5 5) True 

myBaseLayout = Tall 1 (2/100) (1/2) |||
               Mirror (Tall 1 (2/100) (1/2)) |||
               simpleTabbed

myManageHook = composeAll
    [ className =? "Xmessage" --> doFloat
    , title =? "vim-float" --> doFloat
    , manageDocks
    ]

main = do
    status <- spawnPipe "/usr/bin/xmobar"
    xmonad $ docks azertyConfig
        { terminal = myTerminal
        , modMask = mod4Mask
        , manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ mySpacedLayout $ myBaseLayout
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn status
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        } `additionalKeysP` myKeys
