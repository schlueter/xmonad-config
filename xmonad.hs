-- Author: Brandon Schlueter
--
-- Requires yegonesh, clipmenu, and services from my bin repository

import System.IO (hPutStrLn)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86 (
    xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioRaiseVolume
    , xF86XK_AudioNext, xF86XK_AudioPlay , xF86XK_AudioPrev
    , xF86XK_KbdBrightnessDown, xF86XK_KbdBrightnessUp
    , xF86XK_LaunchA, xF86XK_LaunchB
    , xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Util.CustomKeys (customKeys)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

myModMask = mod4Mask
myTerminal = "st tmux attach -t default || st tmux new -s default"
myStatusBar = "dzen2 -dock -p -xs 1 -ta l -e 'onstart=lower'"

splitLayout = avoidStruts $ smartBorders (Tall 1 (3/100) (1/2))
oneAppLayout = avoidStruts $ noBorders (fullscreenFull Full)

myLayoutHook = splitLayout ||| oneAppLayout
myManageHook = manageDocks

myLogHook = defaultPP
    { ppCurrent         = dzenColor "#303030" "#909090" . pad
    , ppHidden          = dzenColor "#909090" "" . pad
    , ppHiddenNoWindows = dzenColor "#606060" "" . pad
    , ppLayout          = dzenColor "#909090" "" . pad
    , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip
    , ppTitle           = shorten 100
    , ppWsSep           = ""
    , ppSep             = "  "
    , ppOutput          = hPutStrLn h}

toggleStrutsKey XConfig {modMask = modMask} = (modMask, xK_h)

inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
inskeys conf@XConfig {modMask = modMask} =
  [ ((modMask,               xK_p        ), spawn "yegonesh")
  , ((modMask,               xK_backslash), spawn "clipmenu")
  , ((modMask,               xK_b        ), spawn "firefox")
  , ((modMask,               xK_l        ), spawn "slock") ]
  ++
  [ ((modMask .|. mod1Mask,   xK_space    ), spawn "cycle-keyboard-layout dvorak us") ]
  ++
  [ ((modMask .|. shiftMask, xK_h        ), sendMessage Shrink) -- Shrink the master area
  , ((modMask .|. shiftMask, xK_l        ), sendMessage Expand) ] -- Expand the master area
  ++
  [ ((0,         xF86XK_AudioPrev        ), spawn "mediac previous")
  , ((0,         xF86XK_AudioPlay        ), spawn "mediac play-pause")
  , ((0,         xF86XK_AudioNext        ), spawn "mediac next")
  , ((0,         xF86XK_AudioMute        ), spawn "mediac toggle-mute")
  , ((0,         xF86XK_AudioLowerVolume ), spawn "mediac -1")
  , ((0,         xF86XK_AudioRaiseVolume ), spawn "mediac +1")
  , ((0,         xF86XK_MonBrightnessUp  ), spawn "brightness +15")
  , ((0,         xF86XK_MonBrightnessDown), spawn "brightness -15")
  , ((shiftMask, xF86XK_AudioLowerVolume ), spawn "mediac -")
  , ((shiftMask, xF86XK_AudioRaiseVolume ), spawn "mediac +") ]

delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig {} = []

myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", "1184", "-h", "16", "-w", "1920", "-ta", "r", "-expand", "l", "-fg", "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""] }

main = do
  dzen <- spawnPipe "dzen2 -dock -p -xs 1 -ta l -e 'onstart=lower'"

  xmonad $ statusBar myStatusBar myLogHook toggleStrutsKey def
     { keys       = customKeys delkeys inskeys
     , layoutHook = myLayoutHook
     , modMask    = myModMask -- ⌘  on macbook pro
     , logHook    = myLogHook dzen
     , terminal	  = myTerminal
     , manageHook = myManageHook }
