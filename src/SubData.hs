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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module SubData where

import ClassyPrelude.Yesod

data SubApp master = SubApp

mkYesodSubData "(YesodSubApp master) => SubApp master" [parseRoutes|
/hello HelloR GET
!/#{Key (Thing master)} ThingR GET
|]

class ( PersistEntity (Thing master)
      , PathPiece (Key (Thing master))
      ) => YesodSubApp master where

  type Thing master

newSubApp :: MonadIO m => m (SubApp site)
newSubApp = pure SubApp
