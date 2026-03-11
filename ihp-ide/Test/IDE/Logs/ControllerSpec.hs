module Test.IDE.Logs.ControllerSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.Logs.Controller

tests :: Spec
tests = do
    describe "extractProcessComposeNames" do
        it "extracts process names from a simple config" do
            let yaml =
                    [ "version: \"0.5\""
                    , "processes:"
                    , "  ihp:"
                    , "    command: start"
                    , "  esbuild:"
                    , "    command: esbuild --watch"
                    , "  tailwind:"
                    , "    command: tailwindcss --watch"
                    ]
            extractProcessComposeNames yaml `shouldBe` ["ihp", "esbuild", "tailwind"]

        it "does not pick up nested keys like command or depends_on" do
            let yaml =
                    [ "processes:"
                    , "  esbuild:"
                    , "    command: esbuild --watch"
                    , "    depends_on:"
                    , "      postgres:"
                    , "        condition: process_healthy"
                    , "  tailwind:"
                    , "    command: tailwindcss"
                    ]
            extractProcessComposeNames yaml `shouldBe` ["esbuild", "tailwind"]

        it "stops at the next top-level key" do
            let yaml =
                    [ "processes:"
                    , "  myapp:"
                    , "    command: run"
                    , "environment:"
                    , "  - FOO=bar"
                    ]
            extractProcessComposeNames yaml `shouldBe` ["myapp"]

        it "returns empty list when no processes section" do
            let yaml =
                    [ "version: \"0.5\""
                    , "environment:"
                    , "  - FOO=bar"
                    ]
            extractProcessComposeNames yaml `shouldBe` []

        it "skips comment lines inside processes" do
            let yaml =
                    [ "processes:"
                    , "  # this is a comment"
                    , "  web:"
                    , "    command: run-web"
                    ]
            extractProcessComposeNames yaml `shouldBe` ["web"]

        it "handles empty processes section" do
            let yaml =
                    [ "processes:"
                    , "other_key:"
                    , "  foo: bar"
                    ]
            extractProcessComposeNames yaml `shouldBe` []

    describe "filterServiceLines" do
        it "extracts lines for a specific service" do
            let logLines =
                    [ "esbuild  | Building..."
                    , "esbuild  | Done in 0.5s"
                    , "tailwind | Rebuilding CSS"
                    , "esbuild  | Watching for changes"
                    ]
            filterServiceLines "esbuild" logLines `shouldBe`
                [ " Building..."
                , " Done in 0.5s"
                , " Watching for changes"
                ]

        it "returns empty list when service has no log lines" do
            let logLines =
                    [ "tailwind | Rebuilding CSS"
                    , "tailwind | Done"
                    ]
            filterServiceLines "esbuild" logLines `shouldBe` []

        it "handles lines without pipe separator" do
            let logLines =
                    [ "esbuild  | Building..."
                    , "some random line"
                    , "esbuild  | Done"
                    ]
            filterServiceLines "esbuild" logLines `shouldBe`
                [ " Building..."
                , " Done"
                ]

        it "handles padded service names (process-compose format)" do
            let logLines =
                    [ "esbuild           | Starting build"
                    , "postgres          | ready to accept connections"
                    ]
            filterServiceLines "esbuild" logLines `shouldBe` [" Starting build"]
            filterServiceLines "postgres" logLines `shouldBe` [" ready to accept connections"]

    describe "filterBuiltinServices" do
        it "removes ihp and postgres" do
            filterBuiltinServices ["ihp", "esbuild", "postgres", "tailwind"]
                `shouldBe` ["esbuild", "tailwind"]

        it "is case-insensitive for builtin names" do
            filterBuiltinServices ["IHP", "Postgres", "esbuild"]
                `shouldBe` ["esbuild"]
