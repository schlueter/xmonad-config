-- Author: Brandon Schlueter

import           Control.Monad

import           XMonad

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName

import           XMonad.Layout.Circle
import           XMonad.Layout.Fullscreen       ( FullscreenFull
                                                , fullscreenFull )
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.NoBorders        ( SmartBorder
                                                , WithBorder
                                                , noBorders
                                                , smartBorders)
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ShowWName

import           XMonad.Util.EZConfig           ( additionalKeysP
                                                , checkKeymap)
import           XMonad.Util.NamedWindows       ( getName )
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

import qualified XMonad.StackSet               as W


main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ docks def {
      layoutHook  = myLayOutHook
    , logHook     = dynamicLogWithPP myPP { ppOutput = hPutStrLn xmproc }
    , manageHook  = manageHook def <+> manageDocks
    , modMask     = mod4Mask -- ⌘  key on mac
    , terminal    = "kitty"
    , startupHook = myStartupHook
    } `additionalKeysP` myKeymap

myPP = def {
    ppCurrent = xmobarColor "#1ABC9C" "" . wrap "[" "]"
  , ppTitle   = xmobarColor "#1ABC9C" "" . shorten 60
  , ppVisible = wrap "(" ")"
  , ppUrgent  = xmobarColor "red" "yellow"
  }

myTabConfig = def {
    activeColor = "#556064"
  , inactiveColor = "#2F3D44"
  , urgentColor = "#FDF6E3"
  , activeBorderColor = "#454948"
  , inactiveBorderColor = "#454948"
  , urgentBorderColor = "#268BD2"
  , activeTextColor = "#80FFF9"
  , inactiveTextColor = "#1ABC9C"
  , urgentTextColor = "#1ABC9C"
  , fontName = "xft:Noto Sans CJK:size=10:antialias=true"
  }

twoColumnLayout = smartBorders $ avoidStruts (Tall 1 (3 / 100) (1 / 2))
oneWindowLayout = avoidStruts (noBorders (fullscreenFull Full))
tabbedLayout = avoidStruts $ noBorders (tabbed shrinkText myTabConfig)
myLayOutHook = twoColumnLayout ||| oneWindowLayout ||| Circle ||| tabbedLayout

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "setxkbmap -layout dvorak -option caps:ctrl_modifier"
    spawnOnce "xcape -e 'Caps_Lock=Escape'"
    spawnOnce "xbanish &"
    spawnOnce "xrandr --output eDP-1 --scale 1x1"
    spawnOnce "picom --daemon --dbus"
    spawnOnce "xbg"
    spawnOnce $ join [
        "trayer"
      , " --edge top"
      , " --align right"
      , " --SetDockType true"
      , " --SetPartialStrut true"
      , " --expand true"
      , " --width 10"
      , " --transparent true"
      , " --alpha 155"
      , " --tint 0x283339"
      , " --height 30"
      , " &" ]
    spawnOnce "blueman-tray &"
    spawnOnce "nm-applet &"
    spawnOnce "psystray &"
    spawnOnce $ join [
        "xautolock"
      , " -time 10"
      , " -notify 120"
      , " -corners"
      , " -0+0"
      , " -locker slock electricsheep"
      , " -detectsleep"
      , " -secure"
      , "&" ]

    setWMName "XMonad"

myKeymap :: [(String, X ())]
myKeymap =
  [ ( "M-M1-<Space>"
    , spawn "cycle-keyboard-layout dvorak us"
    ) -- mod + alt + space
  , ("M-p" , spawn "yegonesh")
  , ("M-\\", spawn "clipmenu")
  , ("M-b" , spawn "firefox")
  , ("M-l" , spawn "slock")
  , ("M-h" , sendMessage ToggleStruts)
  , ( "M-S-h"
    , sendMessage Shrink
    ) -- %! Shrink the master area
  , ( "M-S-l"
    , sendMessage Expand
    ) -- %! Expand the master area
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

