module Bindings.Stemmer
       ( Encoding(..)
       , Language(..)
       , StemConfig(..)
       , Stemmer(..)
       , init_stemmer
       , new_stemmer
       , stemword
       , delete_stemmer
       , unsafeStemword ) where

import Bindings.Stemmer.Raw
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (toLower)
import Foreign.Ptr

-- | 'Encoding' Type
--
--   === NOTE:
--
--   * 'ISO_8859_2' for 'Romanian' | 'Hungarian' only.
--   * 'KOI8_R' for 'Russian' only.
data Encoding = UTF_8
              | ISO_8859_1
              | ISO_8859_2
              | KOI8_R
              deriving Show

-- | 'Language' Type
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

-- | 'Stem' Type
data StemConfig = StemConfig { language :: Language
                 , encoding :: Encoding }
          deriving Show

-- | 'Stemmer' Type
--
--   Wrapper type for Ptr C'sb_stemmer.
type Stemmer = Ptr C'sb_stemmer

-- | create 'Stem' type
--
--   * algorithm: 'Language'
--
--   * encoding: 'Encoding'
init_stemmer :: Language -> Encoding -> IO StemConfig
init_stemmer lang enc = do
  return StemConfig { language = lang
                    , encoding = enc  }

-- | create stemmer instance
new_stemmer :: StemConfig -> IO Stemmer
new_stemmer StemConfig{..} = do
  cword_enc <- encodingCString encoding
  algorithm <- languageCString language
  stemmer <- c'sb_stemmer_new algorithm cword_enc
  return stemmer

-- | stem word with 'Stemmer'
stemword :: Stemmer -> String -> IO String
stemword stemmer word = do
  cword <- newCString word
  strPtr <- c'sb_stemmer_stem stemmer cword (fromIntegral $ length word)
  str_length <- c'sb_stemmer_length stemmer
  peekCStringLen (strPtr, fromIntegral str_length)

-- | delete stemmer instance
delete_stemmer :: Stemmer -> IO ()
delete_stemmer = c'sb_stemmer_delete

-- | stem words with unsafePerformIO
unsafeStemword :: Stemmer -> String -> String
unsafeStemword stemmer word = unsafePerformIO $ stemword stemmer word

-- | 'Encoding' Type Util function
encodingCString :: Encoding -> IO CString
encodingCString = newCString . show

-- | 'Language' Type Util function
languageCString :: Language -> IO CString
languageCString = newCString . go . show
    where go (x:xs) = (toLower x) : xs
