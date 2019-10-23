-- Author: Brandon Schlueter

import Graphics.X11.ExtraTypes.XF86 ( xF86XK_AudioPrev
                                    , xF86XK_AudioPlay
                                    , xF86XK_AudioNext
                                    , xF86XK_AudioMute
                                    , xF86XK_AudioLowerVolume
                                    , xF86XK_AudioRaiseVolume
                                    )

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Util.CustomKeys (customKeys)


main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig

myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "«" "»" }
toggleStrutsKey XConfig {modMask = modMask} = (modMask, xK_h)

myConfig = def { keys = customKeys delkeys inskeys
               , layoutHook = smartBorders $ avoidStruts ( Tall 1 (3/100) (1/2)) ||| noBorders (fullscreenFull Full)
               , modMask = mod4Mask -- ⌘  key on mac
               , terminal = "st tmux attach -t default || st tmux new -s default"
               }

inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
inskeys conf@XConfig {modMask = modMask} =
  [ ((modMask .|. mod1Mask,  xK_space    ), spawn "cycle-keyboard-layout dvorak us") -- mod + alt + space
  , ((modMask,               xK_p        ), spawn "yegonesh")
  , ((modMask,               xK_backslash), spawn "clipmenu")
  , ((modMask,               xK_b        ), spawn "firefox")
  , ((modMask,               xK_l        ), spawn "slock")
  , ((modMask .|. shiftMask, xK_h        ), sendMessage Shrink) -- %! Shrink the master area
  , ((modMask .|. shiftMask, xK_l        ), sendMessage Expand) -- %! Expand the master area
  , ((0,         xF86XK_AudioPrev        ), spawn "mediac previous")
  , ((0,         xF86XK_AudioPlay        ), spawn "mediac play-pause")
  , ((0,         xF86XK_AudioNext        ), spawn "mediac next")
  , ((0,         xF86XK_AudioMute        ), spawn "mediac toggle-mute")
  , ((0,         xF86XK_AudioLowerVolume ), spawn "mediac -1")
  , ((0,         xF86XK_AudioRaiseVolume ), spawn "mediac +1")
  , ((shiftMask, xF86XK_AudioLowerVolume ), spawn "mediac -")
  , ((shiftMask, xF86XK_AudioRaiseVolume ), spawn "mediac +")
  ]

-- Delete no default keys
delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig {} = []
