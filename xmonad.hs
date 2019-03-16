-- Author: Brandon Schlueter

import Graphics.X11.ExtraTypes.XF86 ( xF86XK_PowerOff
                                    , xF86XK_AudioPrev
                                    , xF86XK_AudioPlay
                                    , xF86XK_AudioNext
                                    , xF86XK_AudioMute
                                    , xF86XK_AudioLowerVolume
                                    , xF86XK_AudioRaiseVolume
                                    )

import XMonad -- Too much to list/how do I explicitly include |||?

import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.StackSet (shift, view)
import XMonad.Util.CustomKeys (customKeys)
import XMonad.Util.Run(spawnPipe)


main =
  xmonad $ def
-   { keys = customKeys delkeys inskeys
    { layoutHook = smartBorders $ avoidStruts ( Tall 1 (3/100) (1/2)) ||| noBorders (fullscreenFull Full)
    , modMask = mod4Mask
    , terminal = "exec st zsh -c 'tmux new'"
    }

inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
inskeys conf@XConfig {modMask = modMask} =
  [((modMask, k), a) | (k, a) <-                       -- modMask + key executes arg
    [ (xK_p,         shellPrompt myXPConfig)
    , (xK_backslash, spawn "clipmenu")
    , (xK_b,         spawn "exec firefox")
    ]]
  ++
  [((shiftMask, k), mediaControl m) | (k, m) <-        -- Shift + Media key executes media control
    [ (xF86XK_AudioLowerVolume, "-")
    , (xF86XK_AudioRaiseVolume, "+")
    ]]
  ++
  [((0, k), mediaControl m) | (k, m) <-                -- Media key executes media control
    [ (xF86XK_AudioPrev,        "previous")
    , (xF86XK_AudioPlay,        "play-pause")
    , (xF86XK_AudioNext,        "next")
    , (xF86XK_AudioMute,        "toggle-mute")
    , (xF86XK_AudioLowerVolume, "-1")
    , (xF86XK_AudioRaiseVolume, "+1")
    ]]
  ++
  [ ((modMask .|. controlMask, xK_l),   -- modMask + control + l
      spawn "notifier Locking; xscreensaver-command -lock")
  , ((modMask .|. shiftMask, xK_b),     -- modMask + shift + l
         spawn "exec firefox --private-window")
  , ((modMask .|. mod1Mask, xK_space),   -- modMask + alt + space
    spawn "cycle-keyboard-layout dvorak us")
  -- Power button
  , ((0, xF86XK_PowerOff),                  spawn "false")
  ]

-- Delete no default keys
delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig {} = []

-- Execute `media-control x`
mediaControl :: MonadIO m => String -> m()
mediaControl x = spawn (unwords ["media-control", x])

-- Config
myXPConfig = defaultXPConfig { autoComplete = Just 1000 }
