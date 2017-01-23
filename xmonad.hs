-- Author: Brandon Schlueter

import Graphics.X11.ExtraTypes.XF86
import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Util.CustomKeys
import XMonad.Util.Run(spawnPipe)


myScreensaver = "/usr/bin/gnome-screensaver-command --lock"
myScreenshot = "scrot '%F-%T_$wx$h.png' -e 'mv $f ~/Pictures/Screenshots'"
myFocusedScreenshot = myScreenshot ++ " -u"
myLauncher = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"
myMuteToggle = "pactl set-sink-mute $(pacmd list-sinks | grep '*' | awk '/\\*/{ print $3}') toggle"
myDecreaseVolume = "pactl set-sink-volume $(pacmd list-sinks | grep '*' | awk '/\\*/{ print $3}') -1%"
myIncreaseVolume = "pactl set-sink-volume $(pacmd list-sinks | grep '*' | awk '/\\*/{ print $3}') +1%"
myPreviousMedia = "playerctl previous"
myNextMedia = "playerctl next"
myPlayPause = "playerctl play-pause"
myBrowser = "firefox"
myPrivateBrowser = "firefox --private-window"
clipboardManager = "clipmenu"
toggleInput = "toggle-macbook-trackpad"

delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig {XMonad.modMask = modMask} =
  [ (modMask .|. m, k) | (m, k) <- zip [0, shiftMask] [xK_w, xK_e, xK_r] ]

inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
inskeys conf@(XConfig {XMonad.modMask = modMask}) =
  [ ((modMask .|. controlMask, xK_l),    spawn myScreensaver)
  , ((modMask, xK_p),                    spawn myLauncher)
  , ((modMask, xK_s),                    spawn myFocusedScreenshot)
  , ((modMask .|. shiftMask, xK_s),      spawn myScreenshot)
  , ((modMask, xK_backslash),            spawn clipboardManager)
  , ((modMask, xK_b),                    spawn myBrowser)
  , ((modMask .|. shiftMask, xK_b),      spawn myPrivateBrowser)
  , ((modMask, xK_i),                    spawn toggleInput)
  , ((0, xF86XK_AudioMute),              spawn myMuteToggle)
  , ((0, xF86XK_AudioLowerVolume),       spawn myDecreaseVolume)
  , ((0, xF86XK_AudioRaiseVolume),       spawn myIncreaseVolume)
  , ((0, xF86XK_AudioPrev),              spawn myPreviousMedia)
  , ((0, xF86XK_AudioPlay),              spawn myPlayPause)
  , ((0, xF86XK_AudioNext),              spawn myNextMedia)
  ]

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaultConfig
    { keys = customKeys delkeys inskeys
    , layoutHook = smartBorders $
        avoidStruts ( Tall 1 (3/100) (1/2)
                    ||| tabbed shrinkText defaultTheme
        ) ||| noBorders (fullscreenFull Full)
    , logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn xmproc }
    , manageHook = manageDocks <+> composeAll
        [ className =? "Dialog" --> doFloat
        , className =? "Gimp" --> doFloat
        , className =? "MPlayer" --> doFloat
        , className =? "Steam" --> doFloat
        , className =? "Xmessage" --> doFloat
        , resource =? "desktop_window" --> doIgnore
        , resource =? "gpicview" --> doFloat
        , resource =? "feh" --> doFloat
        , isFullscreen --> doFullFloat
        ]
    , modMask = mod4Mask
    , terminal = "/usr/bin/terminology"
    }
