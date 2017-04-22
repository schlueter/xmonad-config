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


myScreensaver = "/usr/bin/gnome-screensaver-command --lock"
myScreenshot = "scrot '%F-%T_$wx$h.png' -e 'mv $f ~/Pictures/Screenshots'"
myFocusedScreenshot = myScreenshot ++ " -u"
mySelectionScreenshot = myScreenshot ++ " -s"
myLauncher = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*')"
myMediaControl = "media-control "
myNotify = "notifier "
myBrowser = "firefox"
myPrivateBrowser = "firefox --private-window"
clipboardManager = "clipmenu"
toggleInput = "toggle-macbook-trackpad"
powerButton = "/etc/acpi/powerbtn.sh"

delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig {XMonad.modMask = modMask} =
  [ (modMask .|. m, k) | (m, k) <- zip [0, shiftMask] [xK_w, xK_e, xK_r] ]

inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
inskeys conf@(XConfig {XMonad.modMask = modMask}) =
  [ ((modMask .|. controlMask, xK_l), spawn (myNotify ++ "Locking;" ++ myScreensaver))
  , ((modMask, xK_p), spawn myLauncher)
  , ((modMask, xK_s), spawn (myNotify ++ "'Taking selection screenshot';" ++ mySelectionScreenshot))
  , ((modMask .|. controlMask, xK_s), spawn (myNotify ++ "'Taking focused screenshot';" ++ myFocusedScreenshot))
  , ((modMask .|. shiftMask, xK_s), spawn (myNotify ++ "'Taking screenshot';" ++ myScreenshot))
  , ((modMask, xK_backslash), spawn clipboardManager)
  , ((modMask, xK_b), spawn myBrowser)
  , ((modMask .|. shiftMask, xK_b), spawn myPrivateBrowser)
  , ((modMask, xK_i), spawn toggleInput)
  , ((0, xF86XK_PowerOff), spawn powerButton)
  , ((0, xF86XK_AudioPrev), spawn (myMediaControl ++ "previous"))
  , ((0, xF86XK_AudioPlay), spawn (myMediaControl ++ "play-pause"))
  , ((0, xF86XK_AudioNext), spawn (myMediaControl ++ "next"))
  , ((0, xF86XK_AudioMute), spawn (myMediaControl ++ "toggle-mute"))
  , ((0, xF86XK_AudioLowerVolume), spawn (myMediaControl ++ "-1"))
  , ((0, xF86XK_AudioRaiseVolume), spawn (myMediaControl ++ "+1"))
  , ((shiftMask, xF86XK_AudioLowerVolume), spawn (myMediaControl ++ "-"))
  , ((shiftMask, xF86XK_AudioRaiseVolume), spawn (myMediaControl ++ "+"))
  ]
  ++
  -- These have to be added back because they go away for some reason
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myLayout = smartBorders $ avoidStruts (
        Tall 1 (3/100) (1/2)
        ||| tabbed shrinkText defaultTheme { activeColor = "#000000" }
    ) ||| noBorders (fullscreenFull Full)

myManageHook = manageDocks <+> composeAll
        [ className =? "Dialog" --> doFloat
        , className =? "Gimp" --> doFloat
        , className =? "Xmessage" --> doFloat
        , title =? "Password Required" --> doFloat
        , resource =? "desktop_window" --> doIgnore
        , isFullscreen --> doFullFloat ]

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaultConfig
    { focusFollowsMouse = False
    , keys = customKeys delkeys inskeys
    , layoutHook = myLayout
    , logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn xmproc }
    , manageHook = myManageHook
    , modMask = mod4Mask
    , terminal = "/usr/bin/terminology"
    }
