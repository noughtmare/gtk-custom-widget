{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}

module ExampleWindow
  ( ExampleWindow(..)
  , IsExampleWindow
  , toExampleWindow
  )
where

import           Data.GI.Base                   ( GObject
                                                , TypedObject(glibType)
                                                , ManagedPtr(..)
                                                , unsafeCastTo
                                                , set
                                                , new
                                                , AttrOp((:=))
                                                , on
                                                )
import           Data.GI.Base.GObject           ( DerivedGObject(..)
                                                , GObjectClass(..)
                                                , registerGType
                                                )
import qualified Data.GI.Base.Overloading      as O

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )

import           GHC.OverloadedLabels          as OL
                                                ( IsLabel(..) )

import qualified GI.Gtk                        as Gtk

import           MyWidget                       ( MyWidget(..) )

newtype ExampleWindow = ExampleWindow (ManagedPtr ExampleWindow)

instance TypedObject ExampleWindow where
  glibType = registerGType ExampleWindow

instance GObject ExampleWindow

data ExampleWindowPrivate = ExampleWindowPrivate
  { vBox       :: Gtk.VBox
  , myWidget   :: MyWidget
  , buttonBox  :: Gtk.ButtonBox
  , buttonQuit :: Gtk.Button
  }

instance DerivedGObject ExampleWindow where
  type GObjectParentType ExampleWindow = Gtk.Window
  type GObjectPrivateData ExampleWindow = ExampleWindowPrivate

  objectTypeName     = "Example-ExampleWindow"
  objectClassInit    = exampleWindowClassInit
  objectInstanceInit = exampleWindowInstanceInit

instance O.HasParentTypes ExampleWindow

type instance O.ParentTypes ExampleWindow
  = Gtk.Window ': O.ParentTypes Gtk.Window

class (GObject o, O.IsDescendantOf ExampleWindow o) => IsExampleWindow o
instance (GObject o, O.IsDescendantOf ExampleWindow o) => IsExampleWindow o

toExampleWindow :: (MonadIO m, IsExampleWindow o) => o -> m ExampleWindow
toExampleWindow = liftIO . unsafeCastTo ExampleWindow

instance O.HasAttributeList ExampleWindow
type instance O.AttributeList ExampleWindow = O.AttributeList Gtk.Window

type instance O.SignalList ExampleWindow = O.SignalList Gtk.Window

type family ResolveExampleWindowMethod t o where
  ResolveExampleWindowMethod t o = Gtk.ResolveWindowMethod t o

instance
  ( info ~ ResolveExampleWindowMethod t ExampleWindow
  , O.MethodInfo info ExampleWindow p
  ) => OL.IsLabel t (ExampleWindow -> p) where
  fromLabel = O.overloadedMethod @info

exampleWindowClassInit :: Monad m => p -> m ()
exampleWindowClassInit _klass = return ()

exampleWindowInstanceInit
  :: GObjectClass -> ExampleWindow -> IO ExampleWindowPrivate
exampleWindowInstanceInit _klass window = do

  vBox      <- new Gtk.VBox [#orientation := Gtk.OrientationVertical]
  buttonBox <- new Gtk.ButtonBox
                   [#borderWidth := 6, #layoutStyle := Gtk.ButtonBoxStyleEnd]
  myWidget   <- new MyWidget []
  buttonQuit <- new Gtk.Button [#label := "Quit"]

  set
    window
    [ #title := "Custom Widget example"
    , #borderWidth := 6
    , #defaultHeight := 200
    , #defaultWidth := 400
    ]

  #add window vBox

  #packStart vBox myWidget True True 0
  #showAll myWidget

  #packStart vBox buttonBox False False 0
  #packStart vBox buttonQuit False False 0
  -- QUESTION: This hide doesn't quit the application, it only closes
  -- the window, I still have to use CTRL-C after clicking this button.
  _ <- on buttonQuit #clicked (#hide window)

  #showAll window

  return ExampleWindowPrivate { .. }
