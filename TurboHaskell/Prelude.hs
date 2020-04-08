module TurboHaskell.Prelude
( module CorePrelude
, module Data.Text.IO
, module TurboHaskell.HaskellSupport
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
, module TurboHaskell.NameSupport
, module TurboHaskell.ModelSupport
, module Data.TMap
, module Database.PostgreSQL.Simple
, module Data.IORef
, module Data.Time.Format
, null
)
where

import CorePrelude hiding (putStr, putStrLn, print, error)
import Data.Text.IO (putStr, putStrLn)
import TurboHaskell.HaskellSupport
import Data.Default (def, Default (..))
import ClassyPrelude (null)
import Data.UUID (UUID)
import GHC.Records
import qualified Prelude
import qualified Data.Text as Text
import Data.Proxy (Proxy (Proxy))
import Control.Monad (when, unless, mapM, mapM_, forM, forM_, sequence, sequence_, join)
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
import TurboHaskell.NameSupport
import TurboHaskell.ModelSupport (ModelContext, CanUpdate, NormalizeModel, Id, GetTableName, GetModelName, updateRecord, createRecord, deleteRecord, MetaBag (..))
import Data.TMap (TMap)
import Database.PostgreSQL.Simple (FromRow)
import Data.IORef
import Data.Time.Format

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