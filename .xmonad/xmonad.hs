import XMonad
import XMonad.Util.EZConfig (additionalKeys, removeKeys)
import XMonad.Hooks.DynamicLog
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.OneBig
import XMonad.Layout.Spacing
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.DynamicLog
import Graphics.X11.ExtraTypes.XF86
import Data.Ratio ((%))

main = xmonad =<< xmobar myConfig -- statusBar "xmobar" (xmobarPP { ppSort = getSortByXineramaRule }) toggleStrutsKey myConfig

imWindowId =
  Or (And (ClassName "skype") (Not (Role "ConversationsWindow")))
     (And (ClassName "Steam") (Title "Friends"))

tags =
  [ (1, "\xf269")
  , (2, "\xf120")
  , (3, "\xf121")
  , (4, "\xf268")
  , (6, "\xf3ef")
  , (7, "\xf001")
  ]

tagFor i = show i ++
  case lookup i tags of
    Nothing -> ""
    Just t -> ":" ++ t


-- tags =
--   [ (1, "www")
--   , (2, "term")
--   , (3, "dev")
--   , (4, "chrome")
--   , (6, "slack")
--   , (7, "music")
--   ]

-- statusBar :: LayoutClass l Window
--   => String    -- ^ the command line to launch the status bar
--   -> PP        -- ^ the pretty printing options
--   -> (XConfig Layout -> (KeyMask, KeySym))
--                -- ^ the desired key binding to toggle bar visibility
--   -> XConfig l -- ^ the base config
--   -> IO (XConfig (ModifiedLayout AvoidStruts l))
-- statusBar cmd pp k conf = do
--   h <- spawnPipe cmd
--   return $ docks $ conf
--     { layoutHook = avoidStruts (layoutHook conf)
--     , logHook = do
--                   logHook conf
--                   dynamicLogWithPP pp { ppOutput = hPutStrLn h }
--     , keys       = liftM2 M.union keys' (keys conf)
--     }
--   where
--     keys' = (`M.singleton` sendMessage ToggleStruts) . k

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

myConfig = def
  { normalBorderColor = "#623c00"
  , focusedBorderColor = "#ff9c00"
  , borderWidth = 4
  , focusFollowsMouse = False
  , modMask = modSuper
  , manageHook = composeAll [ className =? "milkytracker" --> doFloat
                            , className =? "Slack" --> doShift (tagFor 6)
                            ]
  , layoutHook = spacingWithEdge 8 $ layoutHook def ||| OneBig (4/5) (4/5) ||| Grid ||| gridIM (1 % 7) imWindowId
  -- , layoutHook = layoutHook def ||| OneBig (4/5) (4/5) ||| Grid ||| gridIM (1 % 7) imWindowId
  , workspaces = map tagFor [1..9 :: Int]
  , terminal = "gnome-terminal"
  }
  `removeKeys`
  [ (modSuper, xK_t)
  ]
  `additionalKeys` concat
  [ map timeTrackerKey [0..9]
  , [ ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -4%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +4%")
    , ((modSuper, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
    , ((modSuper, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
    , ((modSuper, xK_x), shellPrompt amberXPConfig)
    , ((modSuper .|. shiftMask .|. controlMask .|.mod4Mask, xK_q), spawn "reboot")
    , ((modSuper .|. shiftMask .|. controlMask, xK_q), spawn "poweroff")
    , ((modSuper .|. controlMask, xK_q), spawn "code -n /home/rtytgat/.xmobarrc /home/rtytgat/.xmonad/xmonad.hs /home/rtytgat/.xmonad/xmonad-session-rc")
    , ((shiftMask .|. controlMask, xK_Print), spawn "gnome-screenshot --area --clipboard")
    , ((shiftMask, xK_Print), spawn "gnome-screenshot --area")
    , ((modSuper, xK_t), screenWorkspace 3 >>= flip whenJust (windows . W.view))
    , ((modSuper .|. shiftMask, xK_t), screenWorkspace 3 >>= flip whenJust (windows . W.shift))
    , ((modSuper, xK_y), withFocused $ windows . W.sink)
    , ((modSuper, xK_Page_Up), spawn "/home/rtytgat/bin/mod-fav")
    ]
  ]
  where
    modSuper = mod4Mask
    timeTrackerKey i = ((modSuper .|. controlMask, xK_0 + i), runInTerm "" $ "fish -c 'timetrack " ++ show i ++ "'")
