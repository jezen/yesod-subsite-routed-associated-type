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

instance (YesodSubApp master, authId ~ Thing master) => YesodSubDispatch (SubApp authId) master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesSubApp)

getHelloR :: Yesod master => SubHandlerFor (SubApp (Thing master)) master Html
getHelloR = liftHandler $ defaultLayout [whamlet|<p>Hello, world!|]

getThingR :: Yesod master => Show authId => authId -> SubHandlerFor (SubApp authId) master Html
getThingR thingId = liftHandler $ defaultLayout [whamlet|<p>#{tshow thingId}|]
