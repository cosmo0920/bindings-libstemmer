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

-- stem words with default setting
-- * algorithm: English
-- * encoding: UTF_8
stemword :: String -> IO String
stemword word = do
  cword <- newCString word
  cword_enc <- encoding UTF_8
  algorithm <- languageCString English
  stemmer <- c'sb_stemmer_new algorithm cword_enc
  strPtr <- c'sb_stemmer_stem stemmer cword (fromIntegral $ length word)
  str_length <- c'sb_stemmer_length stemmer
  peekCStringLen (strPtr, fromIntegral str_length)

-- stem words with unsafePerformIO
unsafeStemword :: String -> String
unsafeStemword = unsafePerformIO . stemword

-- Specify Encode

encoding :: Encoding -> IO CString
encoding = newCString . show

languageCString :: Language -> IO CString
languageCString = newCString . go . show
    where go (x:xs) = (toLower x) : xs
