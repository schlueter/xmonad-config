-- Author: Brandon Schlueter

import Graphics.X11.ExtraTypes.XF86
import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Util.CustomKeys
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)

import qualified XMonad.StackSet as W


data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

myScreensaver = "/usr/bin/gnome-screensaver-command --lock"
myScreenshot = "scrot '%F-%T_$wx$h.png' -e 'mv $f ~/Pictures/Screenshots'"
myFocusedScreenshot = myScreenshot ++ " -u"
myLauncher = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*')"
myMediaControl = "media-control" ++ " "
myNotify = "notify-send -h string:x-canonical-private-synchronous:anything"
myBrowser = "firefox"
myPrivateBrowser = "firefox --private-window"
clipboardManager = "clipmenu"
toggleInput = "toggle-macbook-trackpad"

delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig {XMonad.modMask = modMask} =
  [ (modMask .|. m, k) | (m, k) <- zip [0, shiftMask] [xK_w, xK_e, xK_r] ]

inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
inskeys conf@(XConfig {XMonad.modMask = modMask}) =
  [ ((modMask .|. controlMask, xK_l),    spawn (myNotify ++ " Locking;" ++ myScreensaver))
  , ((modMask, xK_p),                    spawn myLauncher)
  , ((modMask, xK_s),                    spawn myFocusedScreenshot)
  , ((modMask .|. shiftMask, xK_s),      spawn myScreenshot)
  , ((modMask, xK_backslash),            spawn clipboardManager)
  , ((modMask, xK_b),                    spawn myBrowser)
  , ((modMask .|. shiftMask, xK_b),      spawn myPrivateBrowser)
  , ((modMask, xK_i),                    spawn toggleInput)
  , ((0, xF86XK_AudioPrev),              spawn (myMediaControl ++ "previous"))
  , ((0, xF86XK_AudioPlay),              spawn (myMediaControl ++ "play-pause"))
  , ((0, xF86XK_AudioNext),              spawn (myMediaControl ++ "next"))
  , ((0, xF86XK_AudioMute), spawn (myMediaControl ++ "toggle-mute"))
  , ((0, xF86XK_AudioLowerVolume), spawn (myMediaControl ++ "-1%"))
  , ((0, xF86XK_AudioRaiseVolume), spawn (myMediaControl ++ "+1%"))
  , ((0 .|. shiftMask, xF86XK_AudioLowerVolume), spawn (myMediaControl ++ "-10%"))
  , ((0 .|. shiftMask, xF86XK_AudioRaiseVolume), spawn (myMediaControl ++ "+10%"))
  ]
  ++
  -- mod-{w,e,r}, Switch to physical screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  -- These have to be added back because they go away for some reason
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ withUrgencyHook LibNotifyUrgencyHook
         $ defaultConfig
    { keys = customKeys delkeys inskeys
    , layoutHook = smartBorders $
        avoidStruts ( Tall 1 (3/100) (1/2)
                    ||| tabbed shrinkText defaultTheme { activeColor = "#000000" }
        ) ||| noBorders (fullscreenFull Full)
    , logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn xmproc }
    , manageHook = manageDocks <+> composeAll
        [ className =? "Dialog" --> doFloat
        , className =? "Gimp" --> doFloat
        , className =? "Xmessage" --> doFloat
        , title =? "Password Required" --> doFloat
        , resource =? "desktop_window" --> doIgnore
        , isFullscreen --> doFullFloat
        ]
    , modMask = mod4Mask
    , terminal = "/usr/bin/terminology"
    }
