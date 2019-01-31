{-|
Module      : Database.PostgreSQL.Simple.Geometry
Description : Geometry types.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : Leon P Smith <leon@melding-monads.com>
Stability   : experimental

Code taken from https://github.com/lpsmith/postgresql-simple/pull/161
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Foundation.DatabaseSupport.Point (

      Point(..)
    , pointX
    , pointY

    ) where

import           Prelude
import           Control.Applicative
import           Data.Typeable
import           Data.Attoparsec.ByteString.Char8 hiding (Result, char8)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Internal
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.Types
import           Database.PostgreSQL.Simple.TypeInfo as TI
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import           Database.PostgreSQL.Simple.TypeInfo.Macro as TI
import           Data.ByteString.Builder (byteString, char8)



data Point = Point {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Ord, Typeable, Show)

pointX :: Point -> Double
pointX (Point x _) = x

pointY :: Point -> Double
pointY (Point _ y) = y

instance FromField Point where
    fromField f v =
        if typeOid f /= $(inlineTypoid TI.point)
        then returnError Incompatible f ""
        else case v of
               Nothing -> returnError UnexpectedNull f ""
               Just bs ->
                   case parseOnly parser bs of
                     Left  err -> returnError ConversionFailed f err
                     Right val -> pure val
      where
        parser = do
            string "("
            x <- double
            string ","
            y <- double
            string ")"
            return $ Point x y

instance ToField Point where
    toField p = Many $
        (Plain (byteString "point(")) :
        (toField $ pointX p) :
        (Plain (char8 ',')) :
        (toField $ pointY p) :
        [Plain (char8 ')')]