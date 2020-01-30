import Data.List
import Data.Map (fromList)
import System.IO
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.SpawnOn
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe) 
import XMonad.Util.Themes
import qualified XMonad.StackSet as W

-- General Settings
myTerminal = "lxterminal"
myBrowser  = "qutebrowser"
mySreenLockCmd = "dm-tool lock"
myVirtualWSs = ["1:web", "2:misc", "3:dev", "4:sys"]

-- Independent (synced) screens
nextVirtualWSname :: VirtualWorkspace -> VirtualWorkspace
nextVirtualWSname vWorkSpace =
    case elemIndex vWorkSpace myVirtualWSs of
        Nothing -> head myVirtualWSs
        Just i -> myVirtualWSs !! (mod (i + 1) $ length myVirtualWSs)

prevVirtualWSname :: VirtualWorkspace -> VirtualWorkspace
prevVirtualWSname vWorkSpace =
    case elemIndex vWorkSpace myVirtualWSs of
        Nothing -> head myVirtualWSs
        Just i -> myVirtualWSs !! (mod (i - 1) $ length myVirtualWSs)

liftVirtualWSaction :: 
    (PhysicalWorkspace -> WindowSet -> WindowSet) ->
    (VirtualWorkspace -> VirtualWorkspace) -> WindowSet -> WindowSet
liftVirtualWSaction action wsAction stackset =
    let (screen, vWS) = unmarshall $ W.currentTag stackset in
        let screenAction = action $ marshall screen $ wsAction vWS in
            onScreen screenAction FocusCurrent screen stackset

prevVirtualWS :: WindowSet -> WindowSet
prevVirtualWS = liftVirtualWSaction W.greedyView prevVirtualWSname

shiftPrevVirtualWS :: WindowSet -> WindowSet
shiftPrevVirtualWS = liftVirtualWSaction W.shift prevVirtualWSname

nextVirtualWS :: WindowSet -> WindowSet
nextVirtualWS = liftVirtualWSaction W.greedyView nextVirtualWSname

shiftNextVirtualWS :: WindowSet -> WindowSet
shiftNextVirtualWS = liftVirtualWSaction W.shift nextVirtualWSname

onAllScreens' :: (WindowSet -> WindowSet) -> WindowSet -> WindowSet
onAllScreens' action windowSet =
    foldr (.) id
        [ onScreen action FocusCurrent screen
            | screen <- map S [0..(length $ W.screens windowSet) - 1]
        ] $ windowSet

onAllScreens :: (PhysicalWorkspace -> WindowSet -> WindowSet) -> VirtualWorkspace 
    -> WindowSet -> WindowSet
onAllScreens action workspace windowSet =
    foldr (.) id
        [ onScreen (action (marshall screen workspace)) FocusCurrent screen
            | screen <- map S [0..(length $ W.screens windowSet) - 1]
        ] $ windowSet

wpKeys = ["&","é","\"","'","(","-","è","_","ç","à"]

-- Key bindings
myKeys conf = 
    [ ("M-b", spawn myBrowser)
    , ("M-<Backspace>", kill)
    , ("M-BS", kill)
    , ("M-f", spawn (myTerminal ++ " -e vifm"))
    , ("M-S-l", spawn mySreenLockCmd)
    , ("M-p", spawn "rofi -show drun")
    , ("M-n", onNextNeighbour def W.view)
    , ("M-S-n", composeAll
        [ onNextNeighbour def W.shift
        , onNextNeighbour def W.view
        ])
    , ("M-,", windows $ onAllScreens' prevVirtualWS)
    , ("M-;", windows $ onAllScreens' nextVirtualWS)
    , ("M-S-,", composeAll 
        [ windows $ shiftPrevVirtualWS
        , windows $ onAllScreens' prevVirtualWS
        ])
    , ("M-S-;", composeAll 
        [ windows $ shiftNextVirtualWS
        , windows $ onAllScreens' nextVirtualWS
        ])
    , ("M-C-,", windows $ prevVirtualWS)
    , ("M-C-;", windows $ nextVirtualWS)
    ] ++
    [(("M-S-" ++ key), windows $ onCurrentScreen W.shift workspace)
        | (workspace, key) <- zip (workspaces' conf) wpKeys ] 
    ++
    [(("M-" ++ key), windows $ onAllScreens W.greedyView workspace)
        | (workspace, key) <- zip (workspaces' conf) wpKeys ] 
    ++
    [((mod ++ key), action screen)
        | (screen, key) <- zip [0..] ["a","z","e","r"]
        , (mod, action) <- [("M-", viewScreen def), ("M-S-", sendToScreen def)]]

-- Spacing
mySpacedLayout =
    spacingRaw
        -- Use smart spacing (space only when 2 or more windows are open)
        True 
        -- Screen border
        (Border 0 0 0 0) True 
        -- Window border
        (Border 3 3 3 3) True 

-- Layouts
myTheme = (theme xmonadTheme)
    { activeColor = "#666666"
    , inactiveColor = "#444444"
    , decoHeight = 20
    , fontName = "xft:Courrier:pixelsize=12"
    }

tallLayout = Tall 1 (2/100) (1/2)
tabbedLayout = tabbed shrinkText myTheme

myBaseLayout = tallLayout |||
               Mirror tallLayout |||
               tabbedLayout

-- Floating windows
myManageHook = composeAll
    [ className =? "Xmessage" --> doFloat
    , title =? "vim-float" --> doFloat
    , title =? "mpv-float" --> (doF copyToAll) <+> doFloat
    , manageDocks
    ]

main = do
    status <- spawnPipe "/usr/bin/xmobar"
    nbScreens <- countScreens
    let baseConfig = ewmh $ docks azertyConfig
            { terminal = myTerminal
            , workspaces = withScreens nbScreens myVirtualWSs
            , modMask = mod4Mask
            , focusFollowsMouse = False
            , manageHook = myManageHook <+> manageHook def
            , layoutHook = avoidStruts $ mySpacedLayout $ myBaseLayout
            , logHook = dynamicLogWithPP . marshallPP 0 $ xmobarPP
                { ppOutput = hPutStrLn status
                , ppSep = " - "
                , ppTitle = xmobarColor "orange" "" . shorten 50
                , ppHidden = xmobarColor "white" ""
                , ppHiddenNoWindows = id
                }
            } in xmonad $ additionalKeysP baseConfig $ myKeys baseConfig

