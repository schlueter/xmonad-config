-- Author: Brandon Schlueter
--
-- Requires yegonesh, clipmenu, and services from my bin repository

import Graphics.X11.ExtraTypes.XF86 ( xF86XK_AudioLowerVolume
                                    , xF86XK_AudioMute
                                    , xF86XK_AudioNext
                                    , xF86XK_AudioPlay
                                    , xF86XK_AudioPrev
                                    , xF86XK_AudioRaiseVolume
                                    , xF86XK_KbdBrightnessDown
                                    , xF86XK_KbdBrightnessUp
                                    , xF86XK_LaunchA
                                    , xF86XK_LaunchB
                                    , xF86XK_MonBrightnessDown
                                    , xF86XK_MonBrightnessUp
                                    )

import XMonad
import XMonad.Hooks.DynamicLog (ppCurrent, statusBar, dzenPP, dzenWithFlags, wrap)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Util.CustomKeys (customKeys)
import XMonad.Util.Dzen (dzenConfig)


altMask = mod1Mask

myStatusBar = "dzen2 -dock -expand right"
myPP = dzenPP

splitLayout = smartBorders $ avoidStruts (Tall 1 (3/100) (1/2))
oneAppLayout = noBorders (fullscreenFull Full)

myConfig = def { keys = customKeys delkeys inskeys
               , layoutHook = splitLayout ||| oneAppLayout
               , modMask = mod4Mask -- ⌘  on macbook pro
               , terminal = "st tmux attach -t default || st tmux new -s default"
               , startupHook = spawn "conky | dzen2 -dock -expand left"}

toggleStrutsKey XConfig {modMask = modMask} = (modMask, xK_h)
inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
inskeys conf@XConfig {modMask = modMask} =
  [ ((modMask,               xK_p        ), spawn "yegonesh")
  , ((modMask,               xK_backslash), spawn "clipmenu")
  , ((modMask,               xK_b        ), spawn "firefox")
  , ((modMask,               xK_l        ), spawn "slock") ]
  ++
  [ ((modMask .|. altMask,   xK_space    ), spawn "cycle-keyboard-layout dvorak us") ]
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

-- Delete no default keys
delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig {} = []

-- main = xmonad =<< statusBar myStatusBar myPP toggleStrutsKey myConfig
-- main = xmonad =<< statusBar myStatusBar myPP toggleStrutsKey myConfig
main = xmonad =<< statusBar myStatusBar myPP toggleStrutsKey myConfig
