module Main where

import Test.Hspec
import IHP.OpenAI
import NeatInterpolation (trimming)
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text

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
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
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
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
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
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
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
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
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
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
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
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
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
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
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
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
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
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
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
                                        }
                                    ]
                                , created = 1715593776
                                , model = "gpt-4-turbo-2024-04-09"
                                , systemFingerprint = Just "fp_294de9593d"
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
                                }
                            ]
                        , created = 1715277101
                        , model = "gpt-4-turbo-2024-04-09"
                        , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
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
                                      }
                                  ]
                              , created = 1715277101
                              , model = "gpt-4-turbo-2024-04-09"
                              , systemFingerprint = Just "fp_294de9593d"
                              }
                          ]
                      }

                let parseLines = foldl (\state line -> (parseResponseChunk state (Text.encodeUtf8 line)).state) emptyParserState (Text.lines input)

                parseLines `shouldBe` result