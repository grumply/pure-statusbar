{-# language RankNTypes, LambdaCase, ScopedTypeVariables, DataKinds, TypeApplications, BlockArguments, KindSignatures, NamedFieldPuns, AllowAmbiguousTypes, OverloadedStrings, PostfixOperators #-}
module Pure.Statusbar (Statusbar(..),statusbar,good,bad,neutral,Status(..),Position(..),Simple) where

import Pure.Elm hiding (initial)
import Pure.Data.Txt as Txt (tail)

import Control.Monad.IO.Class (MonadIO(..))
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat,Nat,natVal)

data Status = Good | Neutral | Bad

data Statusbar ty = Statusbar 
  { initial :: Status
  }

data Model = Model
  { current :: Status
  }

data Msg ty = Startup | SetStatus Status

good :: forall ty m. (Typeable ty, MonadIO m) => m ()
good = liftIO (publish (SetStatus @ty Good))

neutral :: forall ty m. (Typeable ty, MonadIO m) => m ()
neutral = liftIO (publish (SetStatus @ty Neutral))

bad :: forall ty m. (Typeable ty, MonadIO m) => m ()
bad = liftIO (publish (SetStatus @ty Bad))

statusbar :: Typeable ty => Statusbar ty -> View
statusbar s = run (App [] [] [] (pure mdl) update view) s
  where
    mdl = Model (initial s)

type Update ty = (Typeable ty, Elm (Msg ty)) => Statusbar ty -> Model -> IO Model

update :: Msg ty -> Update ty
update = \case
  Startup     -> startup
  SetStatus s -> setStatus s

startup :: Update ty
startup _ mdl = do
  subscribe
  pure mdl

setStatus :: Status -> Update ty
setStatus new _ mdl = pure mdl { current = new }

type Render ty = Elm (Msg ty) => Model -> View

view :: Statusbar ty -> Render ty
view _ Model { current } =
  let
    status = \case
      Good    -> Themed @Good
      Neutral -> Themed @Neutral
      Bad     -> Themed @Bad

  in
    Div <| Themed @Statusbar . status current

instance Theme Statusbar
instance Theme Good
instance Theme Neutral
instance Theme Bad

data Position = Relative | Sticky | Absolute

class KnownPosition (position :: Position) where
  posVal :: forall position. Txt

instance KnownPosition Relative where
  posVal = relative

instance KnownPosition Sticky where
  posVal = "sticky"

instance KnownPosition Absolute where
  posVal = absolute

data Simple (position :: Position) (milliseconds :: Nat)

instance (Typeable position, KnownPosition position, KnownNat milliseconds) 
  => Theme (Simple position milliseconds) 
  where
    theme c = do
      let 
        nm = Txt.tail c
        t  = fromIntegral (natVal (Proxy :: Proxy milliseconds))
        p  = posVal @position

      is c do
        has (subtheme @Statusbar) do
          position   =: p
          top        =: 0
          left       =: 0
          width      =: 0
          display    =: block
          transition =* [width,t ms,linear]
       
          at @Neutral do
            display =: none

          at @Good do
            height =: 1em
            width  =: (100%)

          at @Bad do
            height =: 1em
            width  =: (100%)
