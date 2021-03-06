module Bindings.Stemmer.Simple
       ( Stemmer(..)
       , Language(..)
       , Encoding(..)
       , init_stemmer
       , simple_stem ) where

import Bindings.Stemmer (StemConfig(..), Stemmer(..),
                         Language(..), Encoding(..),
                         init_stemmer, new_stemmer,
                         delete_stemmer, stemword)
import Control.Monad.Trans.Resource (allocate, runResourceT, release)
import Control.Monad.Trans.Class (lift)

-- | stem word with 'ResourceT'.
--
--  new & delete 'Stemmer' a.k.a (Ptr C'sb_stemmer) automatically.
simple_stem :: StemConfig -> String -> IO String
simple_stem StemConfig{..} word = runResourceT $ do
  (key, stemmer) <- allocate (new_stemmer StemConfig{..}) delete_stemmer
  str <- lift $ stemword stemmer word
  lift $ release key
  return str
