module Main where

import Test.Hspec
import IHP.OpenAI
import NeatInterpolation (trimming)
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import Data.Aeson

main :: IO ()
main = hspec do
    tests

tests = do
    describe "IHP.OpenAI" do
        describe "parseResponseChunk" do
            it "should parse a simple message response 'What's 1 + 2?'" do
                -- curl https://api.openai.com/v1/chat/completions \
                -- -H "Content-Type: application/json" \
                -- -H "Authorization: Bearer $OPENAI_TOKEN" \
                -- -d '{
                --   "model": "gpt-4-turbo",
                --   "stream": true,
                --   "messages": [
                --     {
                --       "role": "user",
                --       "content": "What 1 + 2?"
                --     }
                --   ]
                -- }'
                let input = [trimming|
                    data: {"id":"chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI","object":"chat.completion.chunk","created":1715593776,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"role":"assistant","content":""},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI","object":"chat.completion.chunk","created":1715593776,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"content":"1"},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI","object":"chat.completion.chunk","created":1715593776,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"content":" +"},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI","object":"chat.completion.chunk","created":1715593776,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"content":" "},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI","object":"chat.completion.chunk","created":1715593776,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"content":"2"},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI","object":"chat.completion.chunk","created":1715593776,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"content":" equals"},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI","object":"chat.completion.chunk","created":1715593776,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"content":" "},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI","object":"chat.completion.chunk","created":1715593776,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"content":"3"},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI","object":"chat.completion.chunk","created":1715593776,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"content":"."},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI","object":"chat.completion.chunk","created":1715593776,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{},"logprobs":null,"finish_reason":"stop"}]}

                    data: [DONE]
                |]
                let result = ParserState
                        { curBuffer = "\n"
                        , emptyLineFound = True
                        , chunks =
                            [ CompletionChunk
                                { id = "chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI"
                                , choices =
                                    [ CompletionChunkChoice
                                        { delta = Delta
                                            { content = Just ""
                                            , toolCalls = Nothing
                                            , role = Just AssistantRole
                                            }
                                        , finishReason = Nothing
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
                                , usage = Nothing
                                }
                            , CompletionChunk
                                { id = "chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI"
                                , choices =
                                    [ CompletionChunkChoice
                                        { delta = Delta
                                            { content = Just "1"
                                            , toolCalls = Nothing
                                            , role = Nothing
                                            }
                                        , finishReason = Nothing
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
                                , usage = Nothing
                                }
                            , CompletionChunk
                                { id = "chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI"
                                , choices =
                                    [ CompletionChunkChoice
                                        { delta = Delta
                                            { content = Just " +"
                                            , toolCalls = Nothing
                                            , role = Nothing
                                            }
                                        , finishReason = Nothing
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
                                , usage = Nothing
                                }
                            , CompletionChunk
                                { id = "chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI"
                                , choices =
                                    [ CompletionChunkChoice
                                        { delta = Delta
                                            { content = Just " "
                                            , toolCalls = Nothing
                                            , role = Nothing
                                            }
                                        , finishReason = Nothing
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
                                , usage = Nothing
                                }
                            , CompletionChunk
                                { id = "chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI"
                                , choices =
                                    [ CompletionChunkChoice
                                        { delta = Delta
                                            { content = Just "2"
                                            , toolCalls = Nothing
                                            , role = Nothing
                                            }
                                        , finishReason = Nothing
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
                                , usage = Nothing
                                }
                            , CompletionChunk
                                { id = "chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI"
                                , choices =
                                    [ CompletionChunkChoice
                                        { delta = Delta
                                            { content = Just " equals"
                                            , toolCalls = Nothing
                                            , role = Nothing
                                            }
                                        , finishReason = Nothing
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
                                , usage = Nothing
                                }
                            , CompletionChunk
                                { id = "chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI"
                                , choices =
                                    [ CompletionChunkChoice
                                        { delta = Delta
                                            { content = Just " "
                                            , toolCalls = Nothing
                                            , role = Nothing
                                            }
                                        , finishReason = Nothing
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
                                , usage = Nothing
                                }
                            , CompletionChunk
                                { id = "chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI"
                                , choices =
                                    [ CompletionChunkChoice
                                        { delta = Delta
                                            { content = Just "3"
                                            , toolCalls = Nothing
                                            , role = Nothing
                                            }
                                        , finishReason = Nothing
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
                                , usage = Nothing
                                }
                            , CompletionChunk
                                { id = "chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI"
                                , choices =
                                    [ CompletionChunkChoice
                                        { delta = Delta
                                            { content = Just "."
                                            , toolCalls = Nothing
                                            , role = Nothing
                                            }
                                        , finishReason = Nothing
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
                                , usage = Nothing
                                }
                            , CompletionChunk
                                { id = "chatcmpl-9OMbIk2dtKfDVvDUNgi8ARVSC4LmI"
                                , choices =
                                    [ CompletionChunkChoice
                                        { delta = Delta
                                            { content = Nothing
                                            , toolCalls = Nothing
                                            , role = Nothing
                                            }
                                        , finishReason = Just FinishReasonStop
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
                                , usage = Nothing
                                }
                            ]
                        }


                let parseLines = foldl (\state line -> (parseResponseChunk state (Text.encodeUtf8 line)).state) emptyParserState (Text.lines input)

                parseLines `shouldBe` result

            it "should parse a line with a function call" do
                let input = Text.encodeUtf8 [trimming|
                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"role":"assistant","content":null,"tool_calls":[{"index":0,"id":"call_cx6RG7DZq3WlIDfXXp9PdtmS","type":"function","function":{"name":"get_current_weather","arguments":""}}]},"logprobs":null,"finish_reason":null}]}
                |]
                let chunk = CompletionChunk
                        { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                        , choices =
                            [ CompletionChunkChoice
                                { delta = Delta
                                    { content = Nothing
                                    , toolCalls = Just [ FunctionCall { index = 0, id = Just "call_cx6RG7DZq3WlIDfXXp9PdtmS", name = Just "get_current_weather", arguments = "" } ]
                                    , role = Just AssistantRole
                                    }
                                , finishReason = Nothing
                                }
                            ]
                        , created = 1715277101
                        , model = "gpt-4-turbo-2024-04-09"
                        , systemFingerprint = Just "fp_294de9593d"
                        , usage = Nothing
                        }
                let result = ParserResult
                        { chunk = Just chunk
                        , state = ParserState
                            { curBuffer = ""
                            , emptyLineFound = False
                            , chunks = [chunk]
                            }
                        }
                parseResponseChunk emptyParserState input `shouldBe` result


            it "should parse a full tool call with arguments" do
                let input = [trimming|
                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"role":"assistant","content":null,"tool_calls":[{"index":0,"id":"call_cx6RG7DZq3WlIDfXXp9PdtmS","type":"function","function":{"name":"get_current_weather","arguments":""}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"{\""}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"location"}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\":\""}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"Boston"}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":","}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":" MA"}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\",\""}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"unit"}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\":\""}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"f"}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"ahrenheit"}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\"}"}}]},"logprobs":null,"finish_reason":null}]}

                    data: {"id":"chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o","object":"chat.completion.chunk","created":1715277101,"model":"gpt-4-turbo-2024-04-09","system_fingerprint":"fp_294de9593d","choices":[{"index":0,"delta":{},"logprobs":null,"finish_reason":"tool_calls"}]}

                    data: [DONE]
                |]
                let result = ParserState
                      { curBuffer = "\n"
                      , emptyLineFound = True
                      , chunks =
                          [ CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Just "call_cx6RG7DZq3WlIDfXXp9PdtmS"
                                                  , name = Just "get_current_weather"
                                                  , arguments = ""
                                                  }
                                              ]
                                          , role = Just AssistantRole
                                          }
                                    , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = "{\""
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = "location"
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = "\":\""
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = "Boston"
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = ","
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = " MA"
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = "\",\""
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = "unit"
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = "\":\""
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = "f"
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = "ahrenheit"
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Just
                                              [ FunctionCall
                                                  { index = 0
                                                  , id = Nothing
                                                  , name = Nothing
                                                  , arguments = "\"}"
                                                  }
                                              ]
                                          , role = Nothing
                                          }
                                      , finishReason = Nothing
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          , CompletionChunk
                              { id = "chatcmpl-9N2DdAg2usc3V0VoinTcCwv5rBs3o"
                              , choices =
                                  [ CompletionChunkChoice
                                      { delta = Delta
                                          { content = Nothing
                                          , toolCalls = Nothing
                                          , role = Nothing
                                          }
                                      , finishReason = Just FinishReasonToolCalls
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              , usage = Nothing
                              }
                          ]
                      }

                let parseLines = foldl (\state line -> (parseResponseChunk state (Text.encodeUtf8 line)).state) emptyParserState (Text.lines input)

                parseLines `shouldBe` result

        describe "ToJSON Tool" do
            it "encode Function call with parameter descriptions" do
                let function = Function
                        { name = "fetchUrl"
                        , description = Just "Fetches a url"
                        , parameters = Just (JsonSchemaObject [ Property { propertyName = "url", type_ = JsonSchemaString, required = True, description = Just "The url to fetch" }])
                        }

                encode function `shouldBe` "{\"function\":{\"description\":\"Fetches a url\",\"name\":\"fetchUrl\",\"parameters\":{\"properties\":{\"url\":{\"description\":\"The url to fetch\",\"type\":\"string\"}},\"type\":\"object\"}},\"type\":\"function\"}"

        describe "ToJSON Message" do
            it "encode a message but only set fields that are not null" do
                let message = userMessage ""

                encode message `shouldBe` "{\"content\":\"\",\"role\":\"user\"}"

        describe "ToJSON JsonSchema" do
            it "not render null description's for object properties" do
                let object = JsonSchemaObject [Property { propertyName = "a", type_ = JsonSchemaString, required = True, description = Nothing } ]

                encode object `shouldBe` "{\"properties\":{\"a\":{\"type\":\"string\"}},\"type\":\"object\"}"

        describe "FromJSON CompletionResult" do
            it "should decode a successful response" do
                let response = [trimming|
                    {
                        "id": "chatcmpl-abc123",
                        "object": "chat.completion",
                        "created": 1677858242,
                        "model": "gpt-3.5-turbo-0613",
                        "usage": {
                            "prompt_tokens": 13,
                            "completion_tokens": 7,
                            "total_tokens": 20
                        },
                        "choices": [
                            {
                                "message": {
                                    "role": "assistant",
                                    "content": "\n\nThis is a test!"
                                },
                                "logprobs": null,
                                "finish_reason": "stop",
                                "index": 0
                            }
                        ]
                    }
                |]

                decodeStrictText (response) `shouldBe` (Just CompletionResult { choices = [ Choice { text = "\n\nThis is a test!" } ] })

            it "should decode an error response" do
                let response = [trimming|
                    {
                        "error": {
                            "message": "You exceeded your current quota, please check your plan and billing details. For more information on this error, read the docs: https://platform.openai.com/docs/guides/error-codes/api-errors.",
                            "type": "insufficient_quota",
                            "param": null,
                            "code": "insufficient_quota"
                        }
                    }
                |]

                decodeStrictText (response) `shouldBe` (Just CompletionError { message = "You exceeded your current quota, please check your plan and billing details. For more information on this error, read the docs: https://platform.openai.com/docs/guides/error-codes/api-errors." })