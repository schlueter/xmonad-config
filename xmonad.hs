-- Author: Brandon Schlueter

import Control.Monad (forM_, join)
import Data.Function (on)
import Data.List (sortBy)
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Util.CustomKeys (customKeys)
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn)

import qualified XMonad.StackSet as W


main = do
    spawn "launch-polybar.sh"
    forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file ->
        safeSpawn "mkfifo" ["/tmp/" ++ file]
    xmonad $ docks $ additionalKeysP myConfig myKeymap

twoColumnLayout = smartBorders $ avoidStruts ( Tall 1 (3/100) (1/2))
oneWindowLayout = avoidStruts (noBorders (fullscreenFull Full))

myConfig = def { layoutHook = twoColumnLayout ||| oneWindowLayout
               , logHook = polybarHook
               , manageHook = manageHook def <+> manageDocks
               , modMask = mod4Mask -- ⌘  key on mac
               , terminal = "kitty"
               , startupHook = return () >> checkKeymap myConfig myKeymap }

-- Populate fifos for polybar
polybarHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let current_workspace = W.currentTag winset
  let workspaces = map W.tag $ W.workspaces winset
  let workspaces_string = join $ map (format current_workspace) $ sort' workspaces

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (workspaces_string ++ "\n")

  where format current_workspace workspace
          | current_workspace == workspace = "%{B#0ff}%{F#000} " ++ workspace ++ " %{B- F-}"
          | otherwise    = " " ++ workspace ++ " "
        sort' = sortBy (compare `on` (!! 0))

myKeymap =
  [ ("M-M1-<Space>"            , spawn "cycle-keyboard-layout dvorak us") -- mod + alt + space
  , ("M-p"                     , spawn "yegonesh")
  , ("M-\\"                    , spawn "clipmenu")
  , ("M-b"                     , spawn "firefox")
  , ("M-l"                     , spawn "slock")
  , ("M-g"                     , goToSelected defaultGSConfig)
  , ("M-h"                     , sendMessage ToggleStruts)
  , ("M-S-h"                   , sendMessage Shrink) -- %! Shrink the master area
  , ("M-S-l"                   , sendMessage Expand) -- %! Expand the master area
  , ("<XF86MonBrightnessUp>"   , spawn "brightness +15")
  , ("<XF86MonBrightnessDown>" , spawn "brightness -15")
  , ("<XF86AudioPrev>"         , spawn "mediac previous")
  , ("<XF86AudioPlay>"         , spawn "mediac play-pause")
  , ("<XF86AudioNext>"         , spawn "mediac next")
  , ("<XF86AudioMute>"         , spawn "mediac toggle-mute")
  , ("<XF86AudioLowerVolume>"  , spawn "mediac -1")
  , ("<XF86AudioRaiseVolume>"  , spawn "mediac +1")
  , ("S-<XF86AudioLowerVolume>", spawn "mediac -")
  , ("S-<XF86AudioRaiseVolume>", spawn "mediac +") ]
