import Bindings.Stemmer.Simple

main :: IO ()
main = do
  line <- getLine
  stemmer <- init_stemmer English UTF_8
  result <- simple_stem stemmer line
  putStrLn result
