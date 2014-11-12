module Bindings.Stemmer where

import Bindings.Stemmer.Raw
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (toLower)

data Encoding = UTF_8
              | ISO_8859_1
              | ISO_8859_2
              | KOI8_R
              deriving Show

data Language = Danish
              | Dutch
              | English
              | Finnish
              | French
              | German
              | Hungarian
              | Italian
              | Norwegian
              | Porter
              | Portuguese
              | Romanian
              | Russian
              | Spanish
              | Swedish
              | Turkish
              deriving Show

data Stem = Stem { language :: Language
                 , encoding :: Encoding }
          deriving Show

init_stem :: Language -> Encoding -> IO Stem
init_stem lang enc = do
  return Stem { language = lang
              , encoding = enc  }

-- stem words with Stem Type and word
-- * algorithm: Language
-- * encoding: Encoding
stemword :: Stem -> String -> IO String
stemword Stem{..} word = do
  cword <- newCString word
  cword_enc <- encodingCString encoding
  algorithm <- languageCString language
  stemmer <- c'sb_stemmer_new algorithm cword_enc
  strPtr <- c'sb_stemmer_stem stemmer cword (fromIntegral $ length word)
  str_length <- c'sb_stemmer_length stemmer
  peekCStringLen (strPtr, fromIntegral str_length)

-- stem words with unsafePerformIO
unsafeStemword :: Stem -> String -> String
unsafeStemword stem word = unsafePerformIO $ stemword stem word

-- Encoding Type Util function
encodingCString :: Encoding -> IO CString
encodingCString = newCString . show

-- Language Type Util function
languageCString :: Language -> IO CString
languageCString = newCString . go . show
    where go (x:xs) = (toLower x) : xs
