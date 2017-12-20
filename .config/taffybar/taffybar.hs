module Main where

import           System.Taffybar
import           System.Taffybar.FreedesktopNotifications
import           System.Taffybar.SimpleClock
import           System.Taffybar.Systray
import           System.Taffybar.TaffyPager

main = do
  let clock = textClockNew Nothing "%a %b %_d %H:%M" 1
      tray = systrayNew
      pager = taffyPagerNew defaultPagerConfig
  defaultTaffybar
    defaultTaffybarConfig
    { startWidgets = [pager]
    , endWidgets = [tray, clock]
    }
