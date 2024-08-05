{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module SubData where

import ClassyPrelude.Yesod

data SubApp authId = SubApp

type MyConstraints a =
  ( Eq a
  , Show a
  , Read a
  , PathPiece a
  )

mkYesodSubData "(MyConstraints authId) => SubApp authId" [parseRoutes|
/hello HelloR GET
!/#{authId} ThingR GET
|]

class (Yesod master, MyConstraints (Thing master)) => YesodSubApp master where
  type Thing master

newSubApp :: MonadIO m => m (SubApp site)
newSubApp = pure SubApp
