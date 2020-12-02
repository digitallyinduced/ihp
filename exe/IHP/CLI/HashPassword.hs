{-|
Module: IHP.CLI.HashPassword
Description: Generate a password hash from the command line via @hash-password@
Copyright: (c) digitally induced GmbH, 2020
-}
module Main where

import IHP.Prelude
import qualified Data.Text.IO as Text
import qualified IHP.AuthSupport.Authentication as Auth

main :: IO ()
main = do
    putStrLn "Enter your password and press enter:"
    plaintextPassword <- Text.getLine
    hashedPassword <- Auth.hashPassword plaintextPassword
    putStrLn hashedPassword