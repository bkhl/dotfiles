module Main where

import           System.Taffybar
import           System.Taffybar.FreedesktopNotifications
import           System.Taffybar.SimpleClock
import           System.Taffybar.Systray

main = do
  let clock = textClockNew Nothing "%a %b %_d %H:%M" 1
      tray = systrayNew
  defaultTaffybar
    defaultTaffybarConfig {startWidgets = [clock], endWidgets = [tray]}
