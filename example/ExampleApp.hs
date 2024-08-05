{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ExampleApp where

import ClassyPrelude.Yesod
import SubApp hiding (Thing)
import SubApp qualified

data App = App
  { appHttpManager :: Manager
  , appSubApp      :: SubApp App
  }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Thing sql=things
  foo Text
  bar Text
|]

mkYesod "App" [parseRoutes|
/ HomeR GET
/sub SubR {SubApp App} appSubApp
|]

getHomeR :: HandlerFor App Html
getHomeR = defaultLayout [whamlet|<p>Home|]

instance Yesod App where
  defaultLayout w = do
    p <- widgetToPageContent w
    withUrlRenderer [hamlet|
      $newline never
      $doctype 5
      <html lang="en">
        <head>
          <title>#{pageTitle p}
          ^{pageHead p}
        <body>
          ^{pageBody p}
    |]

instance YesodSubApp App where
  type Thing App = ExampleApp.Thing

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
  getHttpManager = appHttpManager

makeFoundation :: IO App
makeFoundation = App <$> newManager <*> liftIO newSubApp

makeApplication :: App -> IO Application
makeApplication foundation = do
  appPlain <- toWaiAppPlain foundation
  pure $ defaultMiddlewaresNoLogging appPlain

getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  foundation <- makeFoundation
  app1       <- makeApplication foundation
  pure (3000, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _foundation = pure ()
