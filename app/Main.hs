-- |
-- Module      : Main
-- Description : Main module for cryptogram CLI program.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude                   hiding (putStrLn)

import           Options.Applicative

import           Data.Text.IO              (putStrLn)
import           System.Exit               (exitSuccess)

import qualified Data.Text                 as Text
import qualified Toy.Cryptogram            as Cryptogram
import qualified Toy.Cryptogram.Dictionary as Dictionary
import qualified Toy.Cryptogram.Key        as Key

data Mode = Encrypt | Decrypt deriving (Show, Eq)

data Options = Options
  { mode    :: Mode
  , rawKey  :: Maybe Text.Text
  , message :: Text.Text
  } deriving (Show)

options :: Parser Options
options = Options
  <$> flag Decrypt Encrypt
      (long "encrypt" <> short 'e' <> help "Encrypts the message")
  <*> optional (Text.pack <$> strOption
      (long "key" <> short 'k' <> help "The key to use."))
  <*> (Text.pack <$> strArgument (metavar "MESSAGE"))

optionsInfo :: ParserInfo Options
optionsInfo = info (helper <*> options)
                (fullDesc <>
                 progDesc "Solve or generate cryptograms" <>
                 header   "cryptogram - manipulate cryptograms")

main :: IO ()
main = do
  opts <- execParser optionsInfo
  case (mode opts, Key.parse <$> rawKey opts) of
    (_,       Just Nothing)  -> putStrLn "invalid key" >> exitSuccess
    (Encrypt, Just (Just k)) -> putStrLn $ Cryptogram.encrypt k (message opts)
    (Decrypt, Just (Just k)) -> putStrLn $ Cryptogram.decrypt k (message opts)
    (Encrypt, Nothing) -> encryptWithRandomKey (message opts)
    (Decrypt, Nothing) -> putStrLn "Cannot decrypt without a key."

encryptWithRandomKey :: Text.Text -> IO ()
encryptWithRandomKey msg = do
  key <- Key.generateRandom
  putStrLn $ "random key: " <> Key.humanReadable  key
  putStrLn $ "message:    " <> Cryptogram.encrypt key msg

printSolution :: Key.Key -> Text.Text -> IO ()
printSolution k t =
  putStrLn $ "key: " <> Key.humanReadable k <> " message: " <> t
