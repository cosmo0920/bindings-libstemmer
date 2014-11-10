{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <libstemmer.h>
module Bindings.Stemmer.Raw where
import Foreign.Ptr
#strict_import

{- struct sb_stemmer; -}
#opaque_t struct sb_stemmer
{- typedef unsigned char sb_symbol; -}
#synonym_t sb_symbol , CUChar
#ccall sb_stemmer_list , IO (Ptr CString)
#ccall sb_stemmer_new , CString -> CString -> IO (Ptr <struct sb_stemmer>)
#ccall sb_stemmer_delete , Ptr <struct sb_stemmer> -> IO ()
#ccall sb_stemmer_stem , Ptr <struct sb_stemmer> -> CString -> CInt -> IO CString
#ccall sb_stemmer_length , Ptr <struct sb_stemmer> -> IO CInt
