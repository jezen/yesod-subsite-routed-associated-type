{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module SubApp
  ( module SubData
  , Route (..)
  ) where

import ClassyPrelude.Yesod
import SubData

instance (Yesod master, YesodSubApp master)
  => YesodSubDispatch (SubApp master) master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesSubApp)

getHelloR :: Yesod master => SubHandlerFor (SubApp master) master Html
getHelloR = liftHandler $ defaultLayout [whamlet|<p>Hello, world!|]

getThingR ::
     Yesod master
  => YesodSubApp master
  => Key (Thing master)
  -> SubHandlerFor (SubApp master) master Html
getThingR thingId = liftHandler $ defaultLayout [whamlet|<p>#{toPathPiece thingId}|]
