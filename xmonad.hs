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


myScreensaver = "/usr/bin/gnome-screensaver-command --lock"
mySelectScreenshot = "select-screenshot"
myScreenshot = "screenshot"
myLauncher = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"
myMuteToggle = "pactl set-sink-mute $(pacmd list-sinks | grep '*' | awk '/\\*/{ print $3}') toggle"
myDecreaseVolume = "pactl set-sink-volume $(pacmd list-sinks | grep '*' | awk '/\\*/{ print $3}') -1%"
myIncreaseVolume = "pactl set-sink-volume $(pacmd list-sinks | grep '*' | awk '/\\*/{ print $3}') +1%"
myPreviousMedia = "playerctl previous"
myNextMedia = "playerctl next"
myPlayPause = "playerctl play-pause"
myPointerDevice = "bcm5974"
myBrowser = "firefox"
myPrivateBrowser = "firefox --private-window"
myApp1 = "slack"
myApp2 = "spotify"
clipboardManager = "clipmenu"
toggleInput = "toggle-macbook-trackpad"

tabConfig = defaultTheme
  { activeBorderColor = "#7C7C7C"
  , activeTextColor = "#CEFFAC"
  , activeColor = "#000000"
  , inactiveBorderColor = "#7C7C7C"
  , inactiveTextColor = "#EEEEEE"
  , inactiveColor = "#000000"
  }

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
  , ((modMask .|. controlMask, xK_l),    spawn myScreensaver)
  , ((modMask, xK_p),                    spawn myLauncher)
  , ((modMask .|. shiftMask, xK_p),      spawn mySelectScreenshot)
  , ((modMask .|. controlMask .|. shiftMask, xK_p), spawn myScreenshot)
  , ((modMask .|. shiftMask, xK_k),      kill)
  , ((modMask, xK_space),                sendMessage NextLayout)
  , ((modMask .|. shiftMask, xK_space),  setLayout $ XMonad.layoutHook conf)
  , ((modMask, xK_n),                    refresh)
  , ((modMask, xK_Tab),                  windows W.focusDown)
  , ((modMask .|. shiftMask, xK_Tab),    windows W.focusUp)
  , ((modMask, xK_m),                    windows W.focusMaster)
  , ((modMask, xK_Return),               windows W.swapMaster)
  , ((modMask .|. controlMask, xK_j),    windows W.swapDown)
  , ((modMask .|. controlMask, xK_k),    windows W.swapUp)
  , ((modMask, xK_backslash),            spawn clipboardManager)
  , ((modMask, xK_h),                    sendMessage Shrink)
  , ((modMask, xK_l),                    sendMessage Expand)
  , ((modMask, xK_t),                    withFocused $ windows . W.sink)
  , ((modMask, xK_comma),                sendMessage (IncMasterN 1))
  , ((modMask, xK_period),               sendMessage (IncMasterN (-1)))
  , ((modMask .|. shiftMask, xK_q),      io (exitWith ExitSuccess))
  , ((modMask, xK_q),                    restart "xmonad" True)
  , ((modMask, xK_b),                    spawn myBrowser)
  , ((modMask .|. shiftMask, xK_b),      spawn myPrivateBrowser)
  , ((modMask, xK_i),                    spawn toggleInput)
  , ((modMask .|. controlMask, xK_1),    spawn myApp1)
  , ((modMask .|. controlMask, xK_2),    spawn myApp2)
  , ((0, xF86XK_AudioMute),              spawn myMuteToggle)
  , ((0, xF86XK_AudioLowerVolume),       spawn myDecreaseVolume)
  , ((0, xF86XK_AudioRaiseVolume),       spawn myIncreaseVolume)
  , ((0, 0x1008FF16),                    spawn myPreviousMedia)
  , ((0, 0x1008FF14),                    spawn myPlayPause)
  , ((0, 0x1008FF17),                    spawn myNextMedia)
  ] ++
  [((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
  ]

myManageHook = composeAll
      [ className =? "Chromium" --> doShift "2:web"
      , className =? "Dialog" --> doFloat
      , className =? "Gimp" --> doFloat
      , className =? "terminology" --> doShift "1:term"
      , className =? "Google-chrome" --> doShift "2:web"
      , className =? "Firefox" --> doShift "2:web"
      , className =? "Slack" --> doShift "3:comms"
      , className =? "Spotify" --> doShift "4:media"
      , className =? "MPlayer" --> doFloat
      , className =? "Steam" --> doFloat
      , resource =? "desktop_window" --> doIgnore
      , resource =? "gpicview" --> doFloat
      , isFullscreen --> (doF W.focusDown <+> doFullFloat)
      ]

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaults
    { logHook = dynamicLogWithPP $ xmobarPP {
        ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "#FFB6B0" "" . shorten 100
      , ppCurrent = xmobarColor "#CEFFAC" ""
      , ppSep = "   "
      }
    , manageHook = manageDocks <+> myManageHook
    }

defaults = defaultConfig
  { borderWidth = 1
  , clickJustFocuses = False
  -- , clientMask =
  , focusFollowsMouse = False
  , focusedBorderColor = "#ffb6b0"
  -- , handleEventHook =
  -- , handleExtraArgs =
  , keys = myKeys
  , layoutHook = smartBorders $ avoidStruts (
      Tall 1 (3/100) (1/2) |||
      Mirror (Tall 1 (3/100) (1/2)) |||
      tabbed shrinkText tabConfig |||
      spiral (6/7)
    ) ||| noBorders (fullscreenFull Full)
  , manageHook = myManageHook
  , modMask = mod4Mask
  , mouseBindings = myMouseBindings
  , normalBorderColor = "#7c7c7c"
  , terminal = "/usr/bin/terminology"
  , workspaces = ["1:term","2:web","3:comms","4:media"] ++ map show [5..9]
}
