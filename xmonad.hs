import Data.Map (fromList)
import Data.List
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.OnScreen
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe) 
import System.IO

myTerminal = "lxterminal"
myBrowser  = "qutebrowser"
mySreenLockCmd = "dm-tool lock"
myVirtualWSs = ["1:dev", "2:web", "3:misc", "4:sys"]

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

liftVirtualWSaction :: (VirtualWorkspace -> VirtualWorkspace) -> WindowSet -> WindowSet
liftVirtualWSaction wsAction stackset =
    let (screen, vWS) = unmarshall $ W.currentTag stackset in
        let action = W.greedyView $ marshall screen $ wsAction vWS in
            onScreen action FocusCurrent screen stackset

prevVirtualWS :: WindowSet -> WindowSet
prevVirtualWS = liftVirtualWSaction prevVirtualWSname

nextVirtualWS :: WindowSet -> WindowSet
nextVirtualWS = liftVirtualWSaction nextVirtualWSname

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

myKeys conf = 
    [ ("M-b", spawn myBrowser)
    , ("M-w", kill)
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
    , ("M-S-,", windows $ prevVirtualWS)
    , ("M-S-;", windows $ nextVirtualWS)
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
    nbScreens <- countScreens
    let baseConfig = ewmh $ docks azertyConfig
            { terminal = myTerminal
            , workspaces = withScreens nbScreens myVirtualWSs
            , modMask = mod4Mask
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

