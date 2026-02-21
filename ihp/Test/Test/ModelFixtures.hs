{-|
Module: Test.ModelFixtures
Description: Shared model types for test specs
Copyright: (c) digitally induced GmbH, 2020
-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Test.ModelFixtures where

import IHP.Prelude
import IHP.ModelSupport
import IHP.Hasql.FromRow (FromRowHasql(..), HasqlDecodeColumn(..))
import IHP.Job.Types (JobStatus(..))
import IHP.Job.Queue ()
import qualified IHP.HSX.Attribute as HSX

-- | Per-table Id newtypes for test models.
-- These are shared across all test modules.

newtype PostId = PostId UUID
    deriving newtype (Eq, Ord, Show)
type instance Id' "posts" = PostId
instance IdNewtype PostId UUID where
    toId = PostId
    fromId (PostId x) = x
instance Default PostId where def = PostId def

newtype TagId = TagId UUID
    deriving newtype (Eq, Ord, Show)
type instance Id' "tags" = TagId
instance IdNewtype TagId UUID where
    toId = TagId
    fromId (TagId x) = x
instance Default TagId where def = TagId def

newtype WeirdTagId = WeirdTagId UUID
    deriving newtype (Eq, Ord, Show)
type instance Id' "weird_tags" = WeirdTagId
instance IdNewtype WeirdTagId UUID where
    toId = WeirdTagId
    fromId (WeirdTagId x) = x
instance Default WeirdTagId where def = WeirdTagId def

newtype TaggingId = TaggingId UUID
    deriving newtype (Eq, Ord, Show)
type instance Id' "taggings" = TaggingId
instance IdNewtype TaggingId UUID where
    toId = TaggingId
    fromId (TaggingId x) = x
instance Default TaggingId where def = TaggingId def

newtype UserId = UserId UUID
    deriving newtype (Eq, Ord, Show)
type instance Id' "users" = UserId
instance IdNewtype UserId UUID where
    toId = UserId
    fromId (UserId x) = x
instance Default UserId where def = UserId def
instance InputValue UserId where inputValue (UserId x) = inputValue x
instance HSX.ApplyAttribute UserId where
    applyAttribute attr attr' (UserId x) h = HSX.applyAttribute attr attr' (inputValue x) h
instance IsString UserId where
    fromString str = case parsePrimaryKey (cs str) of
        Just pk -> UserId pk
        Nothing -> error ("Unable to convert " <> show str <> " to UserId")

newtype BackgroundJobId = BackgroundJobId UUID
    deriving newtype (Eq, Ord, Show)
type instance Id' "background_jobs" = BackgroundJobId
instance IdNewtype BackgroundJobId UUID where
    toId = BackgroundJobId
    fromId (BackgroundJobId x) = x
instance Default BackgroundJobId where def = BackgroundJobId def

data Post = Post
        { id :: UUID
        , title :: Text
        , externalUrl :: Maybe Text
        , createdAt :: UTCTime
        , public :: Bool
        , createdBy :: UUID
        , categoryId :: Maybe UUID
        }

type instance GetTableName Post = "posts"
type instance GetModelByTableName "posts" = Post
type instance PrimaryKey "posts" = UUID

instance Table Post where
    columnNames = ["id", "title", "external_url", "created_at", "public", "created_by", "category_id"]
    primaryKeyColumnNames = ["id"]

instance FromRowHasql Post where
    hasqlRowDecoder = Post
        <$> hasqlColumnDecoder
        <*> hasqlColumnDecoder
        <*> hasqlColumnDecoder
        <*> hasqlColumnDecoder
        <*> hasqlColumnDecoder
        <*> hasqlColumnDecoder
        <*> hasqlColumnDecoder

data WeirdPkTag = WeirdPkTag
        { tagIden :: UUID
        , tagText :: Text
        }

type instance GetTableName WeirdPkTag = "weird_tags"
type instance GetModelByTableName "weird_tags" = WeirdPkTag
type instance PrimaryKey "weird_tags" = UUID

instance Table WeirdPkTag where
    columnNames = ["tag_iden", "tag_text"]
    primaryKeyColumnNames = ["tag_iden"]

data Tag = Tag
        { id :: UUID
        , tagText :: Text
        }

type instance GetTableName Tag = "tags"
type instance GetModelByTableName "tags" = Tag
type instance PrimaryKey "tags" = UUID

instance Table Tag where
    columnNames = ["id", "tag_text"]
    primaryKeyColumnNames = ["id"]

data Tagging = Tagging
        { id :: UUID
        , postId :: UUID
        , tagId :: UUID
        }

type instance GetTableName Tagging = "taggings"
type instance GetModelByTableName "taggings" = Tagging
type instance PrimaryKey "taggings" = UUID

instance Table Tagging where
    columnNames = ["id", "post_id", "tag_id"]
    primaryKeyColumnNames = ["id"]

data CompositeTagging = CompositeTagging
        { postId :: UUID
        , tagId :: UUID
        }

type instance GetTableName CompositeTagging = "composite_taggings"
type instance GetModelByTableName "composite_taggings" = CompositeTagging
type instance PrimaryKey "composite_taggings" = (PostId, TagId)

instance Table CompositeTagging where
    columnNames = ["post_id", "tag_id"]
    primaryKeyColumnNames = ["post_id", "tag_id"]

data User = User
    { id :: UUID
    , name :: Text
    }

type instance GetTableName User = "users"
type instance GetModelByTableName "users" = User
type instance PrimaryKey "users" = UUID

instance Table User where
    columnNames = ["id", "name"]
    primaryKeyColumnNames = ["id"]

instance FromRowHasql User where
    hasqlRowDecoder = User
        <$> hasqlColumnDecoder
        <*> hasqlColumnDecoder

data FavoriteTitle = FavoriteTitle
    { title :: Text
    , likes :: Int
    }

type instance GetTableName FavoriteTitle = "favorite_title"
type instance GetModelByTableName "favorite_title" = FavoriteTitle

instance Table FavoriteTitle where
    columnNames = ["title", "likes"]
    primaryKeyColumnNames = []

data BackgroundJob = BackgroundJob
    { id :: UUID
    , status :: JobStatus
    }

type instance GetTableName BackgroundJob = "background_jobs"
type instance GetModelByTableName "background_jobs" = BackgroundJob
type instance PrimaryKey "background_jobs" = UUID

instance Table BackgroundJob where
    columnNames = ["id", "status"]
    primaryKeyColumnNames = ["id"]
