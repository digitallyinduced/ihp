module IHP.Prelude
( module CorePrelude
, module Data.Text.IO
, module IHP.HaskellSupport
, module GHC.Records
, UUID
, module Data.Default
, tshow
, Proxy (..)
, module Control.Monad
, module Data.List
, head
, headMay
, last
, lastMay
, show
, module Data.String.Conversions
, module Data.Time.Clock
, module Data.Time.Calendar
, module Data.Text
, module GHC.OverloadedLabels
, plain
, (++)
, error
, module Data.Data
, module GHC.TypeLits
, module IHP.NameSupport
, module IHP.ModelSupport
, module Data.TMap
, module Database.PostgreSQL.Simple
, module Data.IORef
, module Data.Time.Format
, null
, module Control.Exception
)
where

import CorePrelude hiding (putStr, putStrLn, print, error)
import Data.Text.IO (putStr, putStrLn)
import IHP.HaskellSupport
import Data.Default (def, Default (..))
import ClassyPrelude (null)
import Data.UUID (UUID)
import GHC.Records
import qualified Prelude
import qualified Data.Text as Text
import Data.Proxy (Proxy (Proxy))
import Control.Monad (when, unless, mapM, mapM_, forM, forM_, sequence, sequence_, join, forever)
import Data.List hiding (head, last, unwords, unlines, words, lines, isPrefixOf, isSuffixOf, isInfixOf, intercalate, intersperse, (++), splitAt, null)
import qualified Data.List as List
import Data.String.Conversions (ConvertibleStrings (convertString), cs)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Text (words, unwords, lines, unlines, intersperse, intercalate, toLower, toUpper, isInfixOf, isSuffixOf, isPrefixOf, splitAt)
import qualified Data.String.Interpolate
import GHC.OverloadedLabels
import Data.Data (Data)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import IHP.NameSupport
import IHP.ModelSupport (ModelContext, CanUpdate, NormalizeModel, Id, GetTableName, GetModelName, updateRecord, createRecord, deleteRecord, MetaBag (..))
import Data.TMap (TMap)
import Database.PostgreSQL.Simple (FromRow)
import Data.IORef
import Data.Time.Format
import Control.Exception (throw, throwIO, catch)

-- Alias for haskell newcomers :)
a ++ b = a <> b

tshow :: Show a => a -> Text
tshow value = Text.pack (Prelude.show value)

show :: Show a => a -> Text
show = tshow

error :: Text -> a
error message = Prelude.error (Text.unpack message)

head :: [a] -> Maybe a
head [] = Nothing
head list = Just (List.head list)

headMay :: [a] -> Maybe a
headMay = head

last :: [a] -> Maybe a
last [] = Nothing
last list = Just (List.last list)

lastMay :: [a] -> Maybe a
lastMay = last

plain = Data.String.Interpolate.i