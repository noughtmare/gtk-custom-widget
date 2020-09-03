{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main
  ( main
  )
where

import           Data.GI.Base
import qualified GI.Gtk                        as Gtk
import qualified GI.Gio                        as Gio

import           ExampleWindow

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  w <- new ExampleWindow []

  #addWindow app w

  return ()

main :: IO ()
main = do
  app <- new
    Gtk.Application
    [ #applicationId := "example.custom-widget"
    , #flags := [Gio.ApplicationFlagsFlagsNone]
    ]

  _ <- on app #activate (activateApp app)
  _ <- #run app Nothing

  return ()
