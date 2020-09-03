{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE RecordWildCards #-}

module MyWidget
  ( MyWidget(..)
  , IsMyWidget
  , toMyWidget
  )
where

import           Data.GI.Base                   ( GObject
                                                , TypedObject(glibType)
                                                , ManagedPtr(..)
                                                , unsafeCastTo
                                                , withTransient
                                                , get
                                                , on
                                                , set
                                                , new
                                                , AttrOp((:=), (:=>), (:&=))
                                                , GError(..)
                                                , gerrorMessage
                                                )
import           Data.GI.Base.GObject           ( DerivedGObject(..)
                                                , GObjectClass(..)
                                                , registerGType
                                                , gobjectModifyPrivateData
                                                , gobjectGetPrivateData
                                                )
import           Data.GI.Base.GParamSpec        ( PropertyInfo(..)
                                                , gParamSpecValue
                                                )
import           Data.GI.Base.ManagedPtr        ( withManagedPtr
                                                , castTo
                                                )
import qualified Data.GI.Base.Overloading      as O
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )

import           GHC.OverloadedLabels          as OL
                                                ( IsLabel(..) )

import qualified GI.Gtk                        as Gtk
import qualified GI.Gdk                        as Gdk
import qualified GI.Gio                         ( )
import qualified GI.Cairo                      as Cairo

import           Graphics.Rendering.Cairo       ( lineTo
                                                , moveTo
                                                , stroke
                                                , Render
                                                )
import           Graphics.Rendering.Cairo.Internal
                                                ( Render(runRender) )
import           Graphics.Rendering.Cairo.Types ( Cairo(Cairo) )
import           Foreign.Ptr                    ( castPtr )

import           Data.Int                       ( Int32 )
import           Data.Coerce                    ( coerce )
import           Data.IORef                     ( atomicWriteIORef
                                                , newIORef
                                                , readIORef
                                                , IORef
                                                )

import           Control.Monad.Reader           ( ReaderT(runReaderT) )

import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           System.IO                      ( stderr
                                                , hPutStr
                                                , hPutStrLn
                                                )
import qualified Data.Text.IO                  as T

import           Data.Maybe                     ( fromJust )
import           Data.Bits                      ( Bits((.|.)) )
import           Control.Monad                  ( join
                                                , (<=<)
                                                )

newtype MyWidget = MyWidget (ManagedPtr MyWidget)

instance TypedObject MyWidget where
  glibType = registerGType MyWidget

instance GObject MyWidget

data MyWidgetPrivate = MyWidgetPrivate
  { scaleRef       :: IORef Int
  , refGdkWindow   :: Maybe Gdk.Window
  , refCssProvider :: Gtk.CssProvider
  }

instance DerivedGObject MyWidget where
  type GObjectParentType MyWidget = Gtk.Widget
  type GObjectPrivateData MyWidget = MyWidgetPrivate

  objectTypeName     = "Example-MyWidget"
  objectClassInit    = myWidgetClassInit
  objectInstanceInit = myWidgetInstanceInit

instance O.HasParentTypes MyWidget

type instance O.ParentTypes MyWidget = Gtk.Widget ': O.ParentTypes Gtk.Widget

class (GObject o, O.IsDescendantOf MyWidget o) => IsMyWidget o
instance (GObject o, O.IsDescendantOf MyWidget o) => IsMyWidget o

toMyWidget :: (MonadIO m, IsMyWidget o) => o -> m MyWidget
toMyWidget = liftIO . unsafeCastTo MyWidget

instance O.HasAttributeList MyWidget
type instance O.AttributeList MyWidget = O.AttributeList Gtk.Widget

type instance O.SignalList MyWidget = O.SignalList Gtk.Widget

type family ResolveMyWidgetMethod t o where
  ResolveMyWidgetMethod t o = Gtk.ResolveWidgetMethod t o

instance
  ( info ~ ResolveMyWidgetMethod t MyWidget
  , O.MethodInfo info MyWidget p
  ) => OL.IsLabel t (MyWidget -> p) where
  fromLabel = O.overloadedMethod @info

myWidgetClassInit :: GObjectClass -> IO ()
myWidgetClassInit klass = do
  withTransient Gtk.WidgetClass (coerce klass) $ \widgetClass -> do
    getRequestMode <- fromJust <$> get widgetClass #getRequestMode

    set
      widgetClass
      [ #getPreferredWidth :&= myWidgetGetPreferredWidth
      , #getPreferredHeight :&= myWidgetGetPreferredHeight
      , #getPreferredHeightForWidth :&= myWidgetGetPreferredHeightForWidth
      , #getPreferredWidthForHeight :&= myWidgetGetPreferredWidthForHeight
      , #getRequestMode :&= getRequestMode
      , #sizeAllocate :&= myWidgetOnSizeAllocate
      , #realize :&= myWidgetOnRealize
      , #unrealize :&= myWidgetOnUnrealize
      , #draw :&= myWidgetOnDraw
      ]

    #installStyleProperty widgetClass =<< gParamSpecValue
      (PropertyInfo
        "example_scale"
        "Example scale"
        "Scale of the example"
        -- QUESTION: I really have no idea what I'm doing here. How is this
        -- supposed to be done?
        (\o v -> do
          r <- scaleRef <$> gobjectGetPrivateData (o :: MyWidget)
          atomicWriteIORef r v
        )
        (readIORef . scaleRef <=< gobjectGetPrivateData)
        Nothing
      )

  return ()

myWidgetInstanceInit :: GObjectClass -> MyWidget -> IO MyWidgetPrivate
myWidgetInstanceInit _klass widget = do

  scaleRef <- newIORef 1000
  let refGdkWindow = Nothing

  #setHasWindow widget True

  T.putStrLn . ("GObject name: " <>) =<< #getName widget

  #setName widget "my-widget"

  refCssProvider  <- new Gtk.CssProvider []
  refStyleContext <- #getStyleContext widget

  #addProvider refStyleContext
               refCssProvider
               -- QUESTION: Ugly fromIntegral. Can this be fixed?
               (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

  _ <- on refCssProvider #parsingError myWidgetOnParsingError

  #loadFromPath refCssProvider "custom_gtk.css"
    `Gtk.catchCssProviderError` (\ce _ge ->
                                  hPutStrLn stderr
                                    $  "CssProviderError, loadFromPath failed: "
                                    <> show ce
                                )
    -- QUESTION: The C++ interface can also catch general Glib::Error. What is
    -- the equivalent in haskell-gi?
    `catch`                     (\e ->
                                  hPutStrLn stderr
                                    $  "CssProviderError, loadFromPath failed: "
                                    <> show (e :: SomeException)
                                )


  return MyWidgetPrivate { .. }

myWidgetGetPreferredWidth :: Gtk.Widget -> IO (Int32, Int32)
myWidgetGetPreferredWidth _widget = return (60, 100)

myWidgetGetPreferredHeight :: Gtk.Widget -> IO (Int32, Int32)
myWidgetGetPreferredHeight _widget = return (50, 70)

myWidgetGetPreferredHeightForWidth :: Gtk.Widget -> Int32 -> IO (Int32, Int32)
myWidgetGetPreferredHeightForWidth _widget _width = return (50, 70)

myWidgetGetPreferredWidthForHeight :: Gtk.Widget -> Int32 -> IO (Int32, Int32)
myWidgetGetPreferredWidthForHeight _widget _height = return (60, 100)

myWidgetOnSizeAllocate :: Gtk.Widget -> Gdk.Rectangle -> IO ()
myWidgetOnSizeAllocate widget' r = do
  Just widget <- castTo MyWidget widget'
  #setAllocation widget r

  mayRefGdkWindow <- refGdkWindow <$> gobjectGetPrivateData widget
  case mayRefGdkWindow of
    Nothing -> return ()
    Just refGdkWindow ->
      join
        $   #moveResize refGdkWindow
        <$> get r #x
        <*> get r #y
        <*> get r #width
        <*> get r #height


myWidgetOnRealize :: Gtk.Widget -> IO ()
myWidgetOnRealize widget' = do

  Just widget <- castTo MyWidget widget'

  #setRealized widget True

  scaleRef <- scaleRef <$> gobjectGetPrivateData widget
  s        <- readIORef scaleRef
  putStrLn $ "scale (example_scale from the theme/css-file) is: " ++ show s

  mayRefGdkWindow <- refGdkWindow <$> gobjectGetPrivateData widget
  case mayRefGdkWindow of
    Just _  -> return ()
    Nothing -> do
      attributes <- new Gdk.WindowAttr []

      r          <- #getAllocation widget
      set
        attributes
        [ #x :=> get r #x
        , #y :=> get r #y
        , #width :=> get r #width
        , #height :=> get r #height
        -- QUESTION: This eventmask code is very ugly. Is there a better way?
        , #eventMask
        :=> fromIntegral
        .   foldr (\x xs -> fromEnum x .|. xs) 0
        .   (Gdk.EventMaskExposureMask :)
        <$> get widget #events
        , #windowType := Gdk.WindowTypeChild
        , #wclass := Gdk.WindowWindowClassInputOutput
        ]

      parent <- #getParentWindow widget
      window <- Gdk.windowNew
        parent
        attributes
        [Gdk.WindowAttributesTypeX, Gdk.WindowAttributesTypeY]

      gobjectModifyPrivateData widget (\x -> x { refGdkWindow = Just window })

      #setWindow widget window

      #setUserData window (Just widget)


myWidgetOnUnrealize :: Gtk.Widget -> IO ()
myWidgetOnUnrealize widget' = do
  Just widget <- castTo MyWidget widget'
  gobjectModifyPrivateData widget (\x -> x { refGdkWindow = Nothing })
  #unrealize widget

myWidgetOnDraw :: Gtk.Widget -> Cairo.Context -> IO Bool
myWidgetOnDraw widget' ctx = do
  Just widget <- castTo MyWidget widget'

  scaleRef    <- scaleRef <$> gobjectGetPrivateData widget
  s           <- fromIntegral <$> readIORef scaleRef

  r           <- #getAllocation widget
  x           <- fromIntegral <$> get r #x
  y           <- fromIntegral <$> get r #y
  width       <- fromIntegral <$> get r #width
  height      <- fromIntegral <$> get r #height
  let scaleX = width / s
      scaleY = height / s
  refStyleContext <- #getStyleContext widget

  Gtk.renderBackground refStyleContext ctx x y width height

  state <- #getState refStyleContext
  Gdk.cairoSetSourceRgba ctx =<< #getColor refStyleContext state

  renderWithContext ctx $ do
    moveTo (155 * scaleX) (165 * scaleY)
    lineTo (155 * scaleX) (838 * scaleY)
    lineTo (265 * scaleX) (900 * scaleY)
    lineTo (849 * scaleX) (564 * scaleY)
    lineTo (849 * scaleX) (438 * scaleY)
    lineTo (265 * scaleX) (100 * scaleY)
    lineTo (155 * scaleX) (165 * scaleY)
    moveTo (265 * scaleX) (100 * scaleY)
    lineTo (265 * scaleX) (652 * scaleY)
    lineTo (526 * scaleX) (502 * scaleY)
    moveTo (369 * scaleX) (411 * scaleY)
    lineTo (633 * scaleX) (564 * scaleY)
    moveTo (369 * scaleX) (286 * scaleY)
    lineTo (369 * scaleX) (592 * scaleY)
    moveTo (369 * scaleX) (286 * scaleY)
    lineTo (849 * scaleX) (564 * scaleY)
    moveTo (633 * scaleX) (564 * scaleY)
    lineTo (155 * scaleX) (838 * scaleY)
    stroke

  return True

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: Cairo.Context -> Render () -> IO ()
renderWithContext ct r =
  withManagedPtr ct $ \p -> runReaderT (runRender r) (Cairo (castPtr p))

myWidgetOnParsingError :: Gtk.CssSection -> GError -> IO ()
myWidgetOnParsingError section gerror = do
  T.hPutStrLn stderr . ("parsingError: " <>) =<< gerrorMessage gerror

  file <- #getFile section
  T.hPutStrLn stderr . ("  URI = " <>) =<< #getUri file

  startLine     <- #getStartLine section
  endLine       <- #getEndLine section
  startPosition <- #getStartPosition section
  endPosition   <- #getEndPosition section

  hPutStr stderr $ "  start_line = " <> show (startLine + 1)
  hPutStrLn stderr $ ", end_line = " <> show (endLine + 1)

  hPutStr stderr $ "  start_position = " <> show (startPosition + 1)
  hPutStrLn stderr $ ", end_position = " <> show (endPosition + 1)
