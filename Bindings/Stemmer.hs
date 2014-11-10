module Bindings.Stemmer where

import Bindings.Stemmer.Raw
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

-- stem words with default setting
-- * algorithm: English
-- * encoding: UTF_8
stemword :: String -> IO String
stemword word = do
  cword <- newCString word
  cword_enc <- newCString "UTF_8"
  algorithm <- newCString "English"
  stemmer <- c'sb_stemmer_new algorithm cword_enc
  strPtr <- c'sb_stemmer_stem stemmer cword (fromIntegral $ length word)
  str_length <- c'sb_stemmer_length stemmer
  peekCStringLen (strPtr, fromIntegral str_length)

-- stem words with unsafePerformIO
unsafeStemword :: String -> String
unsafeStemword = unsafePerformIO . stemword
