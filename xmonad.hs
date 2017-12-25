-- Author: Brandon Schlueter

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
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.StackSet (shift, view)
import XMonad.Util.CustomKeys (customKeys)
import XMonad.Util.Run(spawnPipe)


altMask = mod1Mask

main =
  xmonad $ defaultConfig
    { keys = customKeys delkeys inskeys
    , layoutHook = smartBorders $ avoidStruts ( Tall 1 (3/100) (1/2)) ||| noBorders (fullscreenFull Full)
    , modMask = mod4Mask
    , terminal = "exec st"
    }

inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
inskeys conf@XConfig {modMask = modMask} =
  -- modMask + key executes arg
  [((modMask, k), a) | (k, a) <-
    [ (xK_p,         shellPrompt def)
    , (xK_backslash, spawn "clipmenu")
    , (xK_b,         spawn "exec firefox")
    ]]
  ++
  -- Shift + Media key executes media control
  [((shiftMask, k), mediaControl m) | (k, m) <-
    [ (xF86XK_AudioLowerVolume, "-")
    , (xF86XK_AudioRaiseVolume, "+")
    ]]
  ++
  -- Media key executes media control
  [((0, k), mediaControl m) | (k, m) <-
    [ (xF86XK_AudioPrev,        "previous")
    , (xF86XK_AudioPlay,        "play-pause")
    , (xF86XK_AudioNext,        "next")
    , (xF86XK_AudioMute,        "toggle-mute")
    , (xF86XK_AudioLowerVolume, "-1")
    , (xF86XK_AudioRaiseVolume, "+1")
    ]]
  ++
  -- modMask + control + l
  [ ((modMask .|. controlMask, xK_l),       spawn "notifier Locking; xscreensaver-command -lock")
  -- modMask + shift + l
  , ((modMask .|. shiftMask, xK_b),         spawn "chromium-browser --incognito")
  -- modMask + alt + space
  , ((modMask .|. altMask, xK_space),       spawn "cycle-keyboard-layout dvorak us")
  -- Power button
  , ((0, xF86XK_PowerOff),                  spawn "/etc/acpi/powerbtn.sh")
  ]

-- Delete no default keys
delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig {} = []

-- Execute `media-control x`
mediaControl :: MonadIO m => String -> m()
mediaControl x = spawn (unwords ["media-control", x])
