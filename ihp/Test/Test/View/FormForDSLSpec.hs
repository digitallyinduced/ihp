{-|
Module: Test.View.FormForDSLSpec
Copyright: (c) digitally induced GmbH, 2026

Verifies that 'formFor' picks up the per-model 'ModelFormAction' instance
emitted by the @[routes|…|]@ splice. Without that override, the
'OVERLAPPABLE' default would string-mangle the request path against
AutoRoute conventions and produce wrong URLs for lowercase DSL routes.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.View.FormForDSLSpec where

import Test.Hspec
import IHP.FrameworkConfig as FrameworkConfig
import Wai.Request.Params.Middleware (RequestBody (..))
import IHP.HSX.Markup (renderMarkupText)
import IHP.ModelSupport
import Data.Bits ((.|.))
import qualified Data.Dynamic as Dynamic
import qualified Data.UUID as UUID
import qualified Network.Wai as Wai
import IHP.ViewPrelude
import qualified Data.Vault.Lazy as Vault
import qualified IHP.RequestVault
import qualified Data.TMap as TypeMap
import IHP.RouterSupport
import IHP.Router.DSL (routes)
import IHP.Router.Capture (renderCapture, parseCapture)
import IHP.ControllerPrelude (Controller (..), renderPlain)
import Network.HTTP.Types.Method (StdMethod (..))

-- A Project model dedicated to this spec so it doesn't collide with
-- 'Test.View.FormSpec'\'s AutoRoute-style fixture (which asserts
-- @/CreateProject@). Two modules wiring the same model into different
-- routing styles would produce overlapping 'ModelFormAction' instances.
data DslProject' = DslProject
    { id :: (Id' "dsl_projects")
    , title :: Text
    , meta :: MetaBag
    } deriving (Eq, Show)
type DslProject = DslProject'

instance InputValue DslProject where inputValue = recordToInputValue

type instance GetTableName DslProject' = "dsl_projects"
type instance GetModelByTableName "dsl_projects" = DslProject
type instance GetModelName DslProject' = "DslProject"
type instance PrimaryKey "dsl_projects" = UUID

instance Record DslProject where
    {-# INLINE newRecord #-}
    newRecord = DslProject def def def
instance Default (Id' "dsl_projects") where def = Id def

instance SetField "id" DslProject' (Id' "dsl_projects") where
    {-# INLINE setField #-}
    setField newValue (DslProject _ title meta) =
        DslProject newValue title (meta { touchedFields = touchedFields meta .|. 1 })
instance SetField "title" DslProject' Text where
    {-# INLINE setField #-}
    setField newValue (DslProject id title meta) =
        DslProject id newValue (meta { touchedFields = touchedFields meta .|. 2 })
instance SetField "meta" DslProject' MetaBag where
    {-# INLINE setField #-}
    setField newValue (DslProject id title _) = DslProject id title newValue

instance UpdateField "id" DslProject' DslProject' (Id' "dsl_projects") (Id' "dsl_projects") where
    {-# INLINE updateField #-}
    updateField newValue (DslProject _ title meta) =
        DslProject newValue title (meta { touchedFields = touchedFields meta .|. 1 })
instance UpdateField "title" DslProject' DslProject' Text Text where
    {-# INLINE updateField #-}
    updateField newValue (DslProject id title meta) =
        DslProject id newValue (meta { touchedFields = touchedFields meta .|. 2 })
instance UpdateField "meta" DslProject' DslProject' MetaBag MetaBag where
    {-# INLINE updateField #-}
    updateField newValue (DslProject id title _) = DslProject id title newValue

instance FieldBit "id" DslProject' where fieldBit = 1
instance FieldBit "title" DslProject' where fieldBit = 2

-- Controller exercising the standard scaffold shape:
--   nullary CreateDslProjectAction
--   UpdateDslProjectAction { dslProjectId :: Id DslProject }
data DslProjectsController
    = DslProjectsAction
    | NewDslProjectAction
    | EditDslProjectAction { dslProjectId :: Id DslProject }
    | CreateDslProjectAction
    | UpdateDslProjectAction { dslProjectId :: Id DslProject }
    deriving (Eq, Show)

instance Controller DslProjectsController where
    action DslProjectsAction       = renderPlain "index"
    action NewDslProjectAction     = renderPlain "new"
    action EditDslProjectAction {} = renderPlain "edit"
    action CreateDslProjectAction  = renderPlain "create"
    action UpdateDslProjectAction {} = renderPlain "update"

-- Force a TH declaration-group boundary so the controller type is visible
-- to the [routes|…|] splice (mirrors the pattern in DSLQuoterSpec).
$(pure [])

-- The splice will emit:
--   * HasPath / CanRoute for the controller (always)
--   * instance {-# OVERLAPPING #-} ModelFormAction DslProject  (because
--     CreateDslProjectAction and UpdateDslProjectAction match the convention)
[routes|DslProjectsController
GET    /dsl-projects                       DslProjectsAction
GET    /dsl-projects/new                   NewDslProjectAction
POST   /dsl-projects                       CreateDslProjectAction
GET    /dsl-projects/{dslProjectId}/edit   EditDslProjectAction
PATCH  /dsl-projects/{dslProjectId}        UpdateDslProjectAction
|]

tests = do
    describe "IHP.View.FormFor (lowercase routes-DSL)" do
        it "uses the DSL path for new records" do
            context <- createControllerContext
            let ?context = context
            let ?request = ?context.request

            let project = newRecord @DslProject
            let form = formFor project [hsx| {textField #title} |]
            renderMarkupText form `shouldSatisfy` (\t -> "action=\"/dsl-projects\"" `isInfixOf` t)

        it "uses the DSL path with id for existing records" do
            context <- createControllerContext
            let ?context = context
            let ?request = ?context.request

            let savedId :: Id' "dsl_projects"
                savedId = Id (UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0)
            let savedMeta = def { originalDatabaseRecord = Just (Dynamic.toDyn ()) }
            let project = DslProject { id = savedId, title = "Saved", meta = savedMeta }
            let form = formFor project [hsx| {textField #title} |]
            renderMarkupText form `shouldSatisfy`
                (\t -> "action=\"/dsl-projects/12345678-9abc-def0-1234-56789abcdef0\"" `isInfixOf` t)

createControllerContext :: IO ControllerContext
createControllerContext = do
    frameworkConfig <- FrameworkConfig.buildFrameworkConfig (pure ())
    let requestBody = FormBody { params = [], files = [], rawPayload = "" }
    let request = Wai.defaultRequest { Wai.vault = Vault.insert IHP.RequestVault.frameworkConfigVaultKey frameworkConfig
                                                 $ Vault.insert IHP.RequestVault.requestBodyVaultKey requestBody Vault.empty }
    let customFields = TypeMap.insert request TypeMap.empty
    pure FrozenControllerContext { customFields }
