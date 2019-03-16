-- Author: Brandon Schlueter
--
-- Requires yegonesh, clipmenu, and services from my bin repository

import Graphics.X11.ExtraTypes.XF86 ( xF86XK_PowerOff
                                    , xF86XK_AudioPrev
                                    , xF86XK_AudioPlay
                                    , xF86XK_AudioNext
                                    , xF86XK_AudioMute
                                    , xF86XK_AudioLowerVolume
                                    , xF86XK_AudioRaiseVolume
                                    )

-- Too much to list/how do I explicitly include |||?
import XMonad
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Util.CustomKeys (customKeys)


altMask = mod1Mask

main =
  xmonad $ def
    { keys = customKeys delkeys inskeys
    , layoutHook = smartBorders $ avoidStruts ( Tall 1 (3/100) (1/2)) ||| noBorders (fullscreenFull Full)
    , modMask = mod4Mask
    , terminal = "exec st tmux attach || tmux new"
    }

inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
inskeys conf@XConfig {modMask = modMask} =
  [ ((modMask .|. altMask,   xK_space    ), spawn "cycle-keyboard-layout dvorak us")
  , ((modMask,               xK_p        ), spawn "yegonesh")
  , ((modMask,               xK_backslash), spawn "clipmenu")
  , ((modMask,               xK_b        ), spawn "firefox")
  , ((modMask .|. shiftMask, xK_b        ), spawn "firefox --private-window")
  , ((0,         xF86XK_AudioPrev        ), spawn "mediac previous")
  , ((0,         xF86XK_AudioPlay        ), spawn "mediac play-pause")
  , ((0,         xF86XK_AudioNext        ), spawn "mediac next")
  , ((0,         xF86XK_AudioMute        ), spawn "mediac toggle-mute")
  , ((0,         xF86XK_AudioLowerVolume ), spawn "mediac -1")
  , ((0,         xF86XK_AudioRaiseVolume ), spawn "mediac +1")
  , ((shiftMask, xF86XK_AudioLowerVolume ), spawn "mediac -")
  , ((shiftMask, xF86XK_AudioRaiseVolume ), spawn "mediac +")
  , ((modMask, xK_z), spawn "env > /tmp/xmonad-env")

  -- Power button -- WARNING this will not work as the login service rather than X handles the power button
  -- For systemd managed systems see logind.conf(5)
  -- , ((0, xF86XK_PowerOff), spawn "false")
  ]

-- Delete no default keys
delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig {} = []
