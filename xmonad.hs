-- Author: Brandon Schlueter

import Graphics.X11.ExtraTypes.XF86

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.StackSet as SS
import XMonad.Util.CustomKeys
import XMonad.Util.Run(spawnPipe)


myMediaControl = "media-control "

delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig {XMonad.modMask = modMask} =
  [ (modMask .|. m, k) | (m, k) <- zip [0, shiftMask] [xK_w, xK_e, xK_r] ]

inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
inskeys conf@(XConfig {XMonad.modMask = modMask}) =
  [ ((modMask .|. controlMask, xK_l), spawn ("notifier Locking; xscreensaver-command -lock"))
  , ((modMask, xK_p), shellPrompt def)
  , ((modMask, xK_backslash), spawn "clipmenu")
  , ((modMask, xK_b), spawn "firefox")
  , ((modMask .|. shiftMask, xK_b), spawn "firefox --private-window")
  , ((0, xF86XK_PowerOff), spawn "/etc/acpi/powerbtn.sh")
  , ((0, xF86XK_AudioPrev), spawn (myMediaControl ++ "previous"))
  , ((0, xF86XK_AudioPlay), spawn (myMediaControl ++ "play-pause"))
  , ((0, xF86XK_AudioNext), spawn (myMediaControl ++ "next"))
  , ((0, xF86XK_AudioMute), spawn (myMediaControl ++ "toggle-mute"))
  , ((0, xF86XK_AudioLowerVolume), spawn (myMediaControl ++ "-1"))
  , ((0, xF86XK_AudioRaiseVolume), spawn (myMediaControl ++ "+1"))
  , ((shiftMask, xF86XK_AudioLowerVolume), spawn (myMediaControl ++ "-"))
  , ((shiftMask, xF86XK_AudioRaiseVolume), spawn (myMediaControl ++ "+"))
  ]
  ++
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(SS.view, 0), (SS.shift, shiftMask)]]

main = do
  xmonad $ defaultConfig
    { keys = customKeys delkeys inskeys
    , layoutHook = smartBorders $ avoidStruts ( Tall 1 (3/100) (1/2)) ||| noBorders (fullscreenFull Full)
    , modMask = mod4Mask
    , terminal = "/usr/bin/terminology"
    }
