-- Author: Brandon Schlueter

-- Based on:
--   http://github.com/vicfryzel/xmonad-config

import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


myTerminal = "/usr/bin/terminology"
myScreensaver = "/usr/bin/gnome-screensaver-command --lock"
mySelectScreenshot = "select-screenshot"
myScreenshot = "screenshot"
myLauncher = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"
myMuteToggle = "pactl set-sink-mute $(pacmd list-sinks | grep '*' | awk '/\\*/{ print $3}') toggle"
myDecreaseVolume = "pactl set-sink-volume $(pacmd list-sinks | grep '*' | awk '/\\*/{ print $3}') -10%"
myIncreaseVolume = "pactl set-sink-volume $(pacmd list-sinks | grep '*' | awk '/\\*/{ print $3}') +10%"
myPreviousMedia = "playerctl previous"
myNextMedia = "playerctl next"
myPlayPause = "playerctl play-pause"

myWorkspaces = ["1:term","2:web","3:code","4:vm","5:media"] ++ map show [6..9]

myManageHook = composeAll
  [ className =? "Chromium" --> doShift "2:web"
  , className =? "Google-chrome" --> doShift "2:web"
  , resource  =? "desktop_window" --> doIgnore
  , className =? "Galculator" --> doFloat
  , className =? "Dialog" --> doFloat
  , className =? "Steam" --> doFloat
  , className =? "Gimp" --> doFloat
  , resource  =? "gpicview" --> doFloat
  , className =? "MPlayer" --> doFloat
  , className =? "VirtualBox" --> doShift "4:vm"
  , className =? "Xchat" --> doShift "5:media"
  , className =? "stalonetray" --> doIgnore
  , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

myLayout = avoidStruts (
  ThreeColMid 1 (3/100) (1/2) |||
  Tall 1 (3/100) (1/2) |||
  Mirror (Tall 1 (3/100) (1/2)) |||
  tabbed shrinkText tabConfig |||
  Full |||
  spiral (6/7)) |||
  noBorders (fullscreenFull Full)

myBorderWidth = 1
myNormalBorderColor = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

tabConfig = defaultTheme {
  activeBorderColor = "#7C7C7C",
  activeTextColor = "#CEFFAC",
  activeColor = "#000000",
  inactiveBorderColor = "#7C7C7C",
  inactiveTextColor = "#EEEEEE",
  inactiveColor = "#000000"
}

xmobarTitleColor = "#FFB6B0"
xmobarCurrentWorkspaceColor = "#CEFFAC"

myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask .|. shiftMask, xK_Return),            spawn $ XMonad.terminal conf)
  , ((modMask .|. controlMask, xK_l),               spawn myScreensaver)
  , ((modMask, xK_p),                               spawn myLauncher)
  , ((modMask .|. shiftMask, xK_p),                 spawn mySelectScreenshot)
  , ((modMask .|. controlMask .|. shiftMask, xK_p), spawn myScreenshot)
  , ((0, xF86XK_AudioMute),                         spawn myMuteToggle)
  , ((modMask .|. controlMask, xK_m),               spawn myMuteToggle)
  , ((0, xF86XK_AudioLowerVolume),                  spawn myDecreaseVolume)
  , ((modMask .|. controlMask, xK_j),               spawn myDecreaseVolume)
  , ((0, xF86XK_AudioRaiseVolume),                  spawn myIncreaseVolume)
  , ((modMask .|. controlMask, xK_k),               spawn myIncreaseVolume)
  , ((0, 0x1008FF16),                               spawn myPreviousMedia)
  , ((0, 0x1008FF14),                               spawn myPlayPause)
  , ((0, 0x1008FF17),                               spawn myNextMedia)
  , ((modMask .|. shiftMask, xK_c),                 kill)
  , ((modMask, xK_space),                           sendMessage NextLayout)
  , ((modMask .|. shiftMask, xK_space),             setLayout $ XMonad.layoutHook conf)
  , ((modMask, xK_n),                               refresh)
  , ((modMask, xK_Tab),                             windows W.focusDown)
  , ((modMask, xK_j),                               windows W.focusDown)
  , ((modMask, xK_k),                               windows W.focusUp)
  , ((modMask, xK_m),                               windows W.focusMaster)
  , ((modMask, xK_Return),                          windows W.swapMaster)
  , ((modMask .|. shiftMask, xK_j),                 windows W.swapDown)
  , ((modMask .|. shiftMask, xK_k),                 windows W.swapUp)
  , ((modMask, xK_h),                               sendMessage Shrink)
  , ((modMask, xK_l),                               sendMessage Expand)
  , ((modMask, xK_t),                               withFocused $ windows . W.sink)
  , ((modMask, xK_comma),                           sendMessage (IncMasterN 1))
  , ((modMask, xK_period),                          sendMessage (IncMasterN (-1)))
  , ((modMask .|. shiftMask, xK_q),                 io (exitWith ExitSuccess))
  , ((modMask, xK_q),                               restart "xmonad" True)
  ]
  ++
  [((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
  ]

myStartupHook = return ()

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaults {
    logHook = dynamicLogWithPP $ xmobarPP {
        ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
      , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
      , ppSep = "   "
    }
    , manageHook = manageDocks <+> myManageHook
    , startupHook = setWMName "LG3D"
  }


defaults = defaultConfig {
  terminal = myTerminal,
  focusFollowsMouse = myFocusFollowsMouse,
  borderWidth = myBorderWidth,
  modMask = myModMask,
  workspaces = myWorkspaces,
  normalBorderColor = myNormalBorderColor,
  focusedBorderColor = myFocusedBorderColor,

  keys = myKeys,
  mouseBindings = myMouseBindings,

  layoutHook = smartBorders $ myLayout,
  manageHook = myManageHook,
  startupHook = myStartupHook
}
