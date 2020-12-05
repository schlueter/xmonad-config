-- Author: Brandon Schlueter

import           Control.Monad

import           XMonad
import           XMonad.Config.Kde
import           XMonad.Hooks.ManageDocks

import           XMonad.Layout.Circle
import           XMonad.Layout.Fullscreen       ( fullscreenFull )
import           XMonad.Layout.NoBorders        ( noBorders
                                                , smartBorders)
import           XMonad.Layout.Tabbed           ( tabbed )

import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.NamedWindows       ( getName )
import           XMonad.Util.SpawnOnce          ( spawnOnce )


main = do
  xmonad $ kdeConfig {
      layoutHook  = myLayOutHook
    , manageHook  = manageHook kdeConfig <+> manageDocks
    , modMask     = mod4Mask -- ⌘  key on mac
    , startupHook = myStartupHook
    , terminal    = "kitty"
    } `additionalKeysP` myKeymap

twoColumnLayout = smartBorders $ avoidStruts (Tall 1 (3 / 100) (1 / 2))
oneWindowLayout = avoidStruts (noBorders (fullscreenFull Full))
tabbedLayout = avoidStruts $ noBorders (tabbed shrinkText def)
myLayOutHook = twoColumnLayout ||| oneWindowLayout ||| Circle ||| tabbedLayout

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xcape -e 'Caps_Lock=Escape'"
    spawnOnce "xbanish &"
    spawnOnce "dunst &"

myKeymap :: [(String, X ())]
myKeymap =
  [ ( "M-M1-<Space>" , spawn "cycle-keyboard-layout dvorak us")
  , ("M-p" , spawn "yegonesh")
  , ("M-\\", spawn "clipmenu")
  , ("M-b" , spawn "firefox")
  , ("M-l" , spawn "slock")
  , ("M-h" , sendMessage ToggleStruts)
  , ( "M-S-h" , sendMessage Shrink)
  , ( "M-S-l" , sendMessage Expand)
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
