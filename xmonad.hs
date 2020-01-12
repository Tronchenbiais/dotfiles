import Data.Map
import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe) 
import System.IO

myTerminal = "terminator"
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

main = do
    status <- spawnPipe "/usr/bin/xmobar"
    xmonad $ docks azertyConfig
        { terminal = myTerminal
        , modMask = mod4Mask
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ mySpacedLayout $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn status
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        } `additionalKeysP` myKeys
