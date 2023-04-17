# OpenAI

This `ihp-openai` package provides streaming functions to access GPT3 and GPT4 from OpenAI.

The package is designed to work well with IHP AutoRefresh and IHP DataSync. The main function `streamCompletion` uses the OpenAI Chat API. It was created before the Chat API was available, and the later rewritten to work well with GPT-4. That's why it feels a bit like a mix of the completion and the chat api.

API calls are retried up to 10 times. A retry will continue with the already generated output tokens, so that a user will never see that a retry has happend.

## Install

1. Make sure you're running on the latest master version of IHP

2. Open `default.nix` and add `ihp-openai` to your haskell dependencies:

    ```nix
    let
        ihp = ...;
        haskellEnv = import "${ihp}/NixSupport/default.nix" {
            ihp = ihp;
            haskellDeps = p: with p; [
                cabal-install
                base
                wai
                text
                hlint
                p.ihp

                ihp-openai
            ];
            otherDeps = p: with p; [
                # Native dependencies, e.g. imagemagick
            ];
            projectPath = ./.;
        };
    in
        haskellEnv

    ```

## Example

```haskell
module Web.Controller.Questions where

import Web.Controller.Prelude
import Web.View.Questions.Index
import Web.View.Questions.New
import Web.View.Questions.Edit
import Web.View.Questions.Show

import qualified IHP.OpenAI as GPT

instance Controller QuestionsController where
    action QuestionsAction = autoRefresh do
        questions <- query @Question
            |> orderByDesc #createdAt
            |> fetch
        render IndexView { .. }

    action NewQuestionAction = do
        let question = newRecord
                |> set #question "What makes haskell so great?"
        render NewView { .. }

    action CreateQuestionAction = do
        let question = newRecord @Question
        question
            |> fill @'["question"]
            |> validateField #question nonEmpty
            |> ifValid \case
                Left question -> render NewView { .. } 
                Right question -> do
                    question <- question |> createRecord
                    setSuccessMessage "Question created"

                    fillAnswer question

                    redirectTo QuestionsAction

    action DeleteQuestionAction { questionId } = do
        question <- fetch questionId
        deleteRecord question
        setSuccessMessage "Question deleted"
        redirectTo QuestionsAction

fillAnswer :: (?modelContext :: ModelContext) => Question -> IO (Async ())
fillAnswer question = do
    -- Put your OpenAI secret key below:
    let secretKey = "sk-XXXXXXXX"

    -- This should be done with an IHP job worker instead of async
    async do 
        GPT.streamCompletion secretKey (buildCompletionRequest question) (clearAnswer question) (appendToken question)
        pure ()

buildCompletionRequest :: Question -> GPT.CompletionRequest
buildCompletionRequest Question { question } =
    -- Here you can adjust the parameters of the request
    GPT.newCompletionRequest
        { GPT.maxTokens = 512
        , GPT.prompt = [trimming|
                Question: ${question}
                Answer:
        |] }

-- | Sets the answer field back to an empty string
clearAnswer :: (?modelContext :: ModelContext) => Question -> IO ()
clearAnswer question = do
    sqlExec "UPDATE questions SET answer = '' WHERE id = ?" (Only question.id)
    pure ()

-- | Stores a couple of newly received characters to the database
appendToken :: (?modelContext :: ModelContext) => Question -> Text -> IO ()
appendToken question token = do
    sqlExec "UPDATE questions SET answer = answer || ? WHERE id = ?" (token, question.id)
    pure ()
```