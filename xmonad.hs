-- Author: Brandon Schlueter

import           Control.Monad                  ( join )

import           XMonad
import           XMonad.ManageHook              ( title )
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Fullscreen       ( fullscreenFull )
import           XMonad.Layout.NoBorders        ( noBorders
                                                , smartBorders
                                                )
import           XMonad.Util.EZConfig           ( additionalKeysP
                                                , checkKeymap
                                                )
import           XMonad.Util.NamedWindows       ( getName )
import           XMonad.Layout.Circle
import           XMonad.Actions.SimpleDate
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.NoBorders        ( SmartBorder )
import           XMonad.Layout.NoBorders        ( WithBorder )
import           XMonad.Layout.Fullscreen       ( FullscreenFull )

import qualified XMonad.StackSet               as W


-- Launch polybar, obviously
-- Run xmonad, making it aware of docks (like polybar), and providing our config
main :: IO ()
main = xmonad $ docks $ additionalKeysP myConfig myKeymap

myStartUpHook :: X ()
myStartUpHook = do
  return ()
  checkKeymap myConfig myKeymap
  spawn "launch-polybar.sh"


-- Check for Nothing
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

twoColumnLayout
  :: ModifiedLayout SmartBorder (ModifiedLayout AvoidStruts Tall) a
twoColumnLayout = smartBorders $ avoidStruts (Tall 1 (3 / 100) (1 / 2))

oneWindowLayout
  :: ModifiedLayout
       AvoidStruts
       (ModifiedLayout WithBorder (ModifiedLayout FullscreenFull Full))
       Window
oneWindowLayout = avoidStruts (noBorders (fullscreenFull Full))

myConfig
  :: XConfig
       ( Choose
           ( XMonad.Layout.LayoutModifier.ModifiedLayout
               XMonad.Layout.NoBorders.SmartBorder
               (XMonad.Layout.LayoutModifier.ModifiedLayout AvoidStruts Tall)
           )
           ( Choose
               ( XMonad.Layout.LayoutModifier.ModifiedLayout
                   AvoidStruts
                   ( XMonad.Layout.LayoutModifier.ModifiedLayout
                       XMonad.Layout.NoBorders.WithBorder
                       ( XMonad.Layout.LayoutModifier.ModifiedLayout
                           XMonad.Layout.Fullscreen.FullscreenFull
                           Full
                       )
                   )
               )
               Circle
           )
       )
myConfig = def { layoutHook  = twoColumnLayout ||| oneWindowLayout ||| Circle
               , logHook     = polybarHook
               , manageHook  = manageHook def <+> manageDocks
               , modMask     = mod4Mask -- ⌘  key on mac
               , terminal    = "kitty"
               -- Enable keyboard shortcut configuration via simple aliases
               , startupHook = myStartUpHook
               }

-- Formatting for workspace display in polybar
currentFormat :: String -> String
currentFormat tag = "%{B#0ff}%{F#000} " ++ tag ++ " %{B- F-}"
inUseFormat :: String -> String
inUseFormat tag = " %{F#666}" ++ tag ++ " %{B- F-}"
notInUseFormat :: String -> String
notInUseFormat tag = " " ++ tag ++ " "

format :: String -> W.Workspace String l a -> String
format current_workspace workspace
  | current_workspace == W.tag workspace = currentFormat $ W.tag workspace
  | isNothing (W.stack workspace)        = inUseFormat $ W.tag workspace
  | otherwise                            = notInUseFormat $ W.tag workspace

-- Populate fifos for polybar
polybarHook :: X ()
polybarHook = do
  winset <- gets windowset
  title  <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let current_workspace = W.currentTag winset
  let workspaces_string =
        join $ map (format current_workspace) $ W.workspaces winset

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (workspaces_string ++ "\n")

myKeymap :: [(String, X ())]
myKeymap =
  [ ( "M-M1-<Space>"
    , spawn "cycle-keyboard-layout dvorak us"
    ) -- mod + alt + space
  , ("M-p" , spawn "yegonesh")
  , ("M-\\", spawn "clipmenu")
  , ("M-b" , spawn "firefox")
  , ("M-l" , spawn "slock")
  , ("M-d" , date)
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
