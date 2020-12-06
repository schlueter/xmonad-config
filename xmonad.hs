-- Author: Brandon Schlueter <b@schlueter.blue>
import           XMonad
import           XMonad.Hooks.ManageDocks
import           XMonad.Config.Kde              ( kde4Config )

import           XMonad.Layout.Circle
import           XMonad.Layout.Fullscreen       ( fullscreenFull )
import           XMonad.Layout.NoBorders        ( smartBorders)

import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.NamedWindows       ( getName )
import           XMonad.Util.SpawnOnce          ( spawnOnce )


main = xmonad $ baseConfig {
    startupHook = startupHook baseConfig <+> myStartupHook
  , manageHook  = manageHook baseConfig <+> manageDocks
  , layoutHook  = twoColumnLayout ||| oneWindowLayout ||| Circle
  }

baseConfig = kde4Config {
    modMask     = mod4Mask -- ⌘  key on mac
  , terminal    = "kitty"
  } `additionalKeysP` myKeymap

twoColumnLayout = avoidStruts $ smartBorders (Tall 1 (3 / 100) (1 / 2))
oneWindowLayout = avoidStruts $ smartBorders $ fullscreenFull Full

myStartupHook = do
    spawnOnce "xcape -e 'Caps_Lock=Escape'"
    spawnOnce "xbanish &"
    spawnOnce "dunst &"

myKeymap =
  [ ("M-M1-<Space>" , spawn "cycle-keyboard-layout dvorak us")
  , ("M-p"   , spawn "yegonesh")
  , ("M-\\"  , spawn "clipmenu")
  , ("M-b"   , spawn "firefox")
  , ("M-l"   , spawn "dbus-send --session --dest=org.freedesktop.ScreenSaver --type=method_call /org/freedesktop/ScreenSaver org.freedesktop.ScreenSaver.SetActive boolean:true")
  , ("M-h"   , sendMessage ToggleStruts)
  , ("M-S-h" , sendMessage Shrink)
  , ("M-S-l" , sendMessage Expand)
  , ("<XF86MonBrightnessUp>"   , spawn "brightness +15")
  , ("<XF86MonBrightnessDown>" , spawn "brightness -15")
  , ("<XF86AudioPrev>"         , spawn "mediac previous")
  , ("<XF86AudioPlay>"         , spawn "mediac play-pause")
  , ("<XF86AudioNext>"         , spawn "mediac next")
  , ("<XF86AudioMute>"         , spawn "mediac toggle-mute")
  , ("<XF86AudioLowerVolume>"  , spawn "mediac -1")
  , ("<XF86AudioRaiseVolume>"  , spawn "mediac +1")
  , ("S-<XF86AudioLowerVolume>", spawn "mediac -")
  , ("S-<XF86AudioRaiseVolume>", spawn "mediac +")
  ]
