import           Data.Monoid
import           System.Exit
import           System.Taffybar.Hooks.PagerHints (pagerHints)
import           XMonad
import           XMonad.Actions.PhysicalScreens
import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks

import qualified Data.Map                         as M
import qualified XMonad.StackSet                  as W

myTerminal = "x-terminal-emulator"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 2

myModMask = mod4Mask

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myNormalBorderColor = "#cccccc"

myFocusedBorderColor = "#ce2029"

myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
  , ((modm, xK_p), spawn "rofi -modi drun,run,window -show drun")
  , ((modm .|. shiftMask, xK_c), kill)
  , ((modm, xK_space), sendMessage NextLayout)
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  , ((modm, xK_n), refresh)
  , ((modm, xK_Tab), windows W.focusDown)
  , ((modm, xK_j), windows W.focusDown)
  , ((modm, xK_k), windows W.focusUp)
  , ((modm, xK_m), windows W.focusMaster)
  , ((modm, xK_Return), windows W.swapMaster)
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  , ((modm, xK_h), sendMessage Shrink)
  , ((modm, xK_l), sendMessage Expand)
  , ((modm, xK_t), withFocused $ windows . W.sink)
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
  , ((modm .|. shiftMask, xK_q), io exitSuccess)
  , ((modm, xK_q), spawn "xmonad --recompile && xmonad --restart")
  ] ++
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [ ((modm, xK_a), onPrevNeighbour W.view)
  , ((modm, xK_o), onNextNeighbour W.view)
  , ((modm .|. shiftMask, xK_a), onPrevNeighbour W.shift)
  , ((modm .|. shiftMask, xK_o), onNextNeighbour W.shift)
  ] ++
  [ ((modm .|. mask, key), f sc)
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
  , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
  ]

myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ( (modm, button1)
      , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ( (modm, button3)
      , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 2 / 3
    delta = 3 / 100

myManageHook = composeAll []

myEventHook = mempty

myLogHook = return ()

myStartupHook = return ()

main = xmonad $ docks $ ewmh $ pagerHints defaults

defaults =
  def
  { terminal = myTerminal
  , focusFollowsMouse = myFocusFollowsMouse
  , clickJustFocuses = myClickJustFocuses
  , borderWidth = myBorderWidth
  , modMask = myModMask
  , workspaces = myWorkspaces
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , keys = myKeys
  , mouseBindings = myMouseBindings
  , layoutHook = myLayout
  , manageHook = myManageHook
  , handleEventHook = myEventHook
  , logHook = myLogHook
  , startupHook = myStartupHook
  }
