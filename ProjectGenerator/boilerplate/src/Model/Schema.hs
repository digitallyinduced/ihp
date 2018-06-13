module Model.Schema where
import ClassyPrelude (Maybe (..), (<>), Bool (..))
import Foundation.SchemaSupport

database = [
    -- Here you can add your database schema
    --
    -- table "hello_worlds"
    --     + field "id" primaryKey
    --     + field "name" text
    --     + field "active" bool { defaultValue = Just (SqlDefaultValue "true") }
    ]
