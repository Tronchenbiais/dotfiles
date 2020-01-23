import Data.Map
import XMonad
import XMonad.Actions.CycleWS
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
mySreenLockCmd = "dm-tool lock"
myKeys = 
    [ ("M-b", spawn myBrowser)
    , ("M-w", kill)
    , ("M-f", spawn (myTerminal ++ " -e vifm"))
    , ("M-S-l", spawn mySreenLockCmd)
    , ("M-p", spawn "rofi -show drun")
    , ("M-Tab", nextScreen)
    , ("M-S-Tab", prevScreen)
    , ("M-n", nextScreen)
    , ("M-,", nextWS)
    , ("M-;", nextWS)
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
        , workspaces = ["1:dev", "2:web", "3:misc", "4:sys"]
        , modMask = mod4Mask
        , manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ mySpacedLayout $ myBaseLayout
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn status
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        } `additionalKeysP` myKeys
