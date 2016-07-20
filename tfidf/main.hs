import System.Environment as System
import Data.List.Split as Split
import TFIDF as Tfidf

toDocs :: [String] -> [Tfidf.Document]
toDocs [] = []
toDocs (h:t) = (Tfidf.Document (read id) content):toDocs t
                where parts   = Split.splitOn "," h
                      id      = parts !! 0
                      content = parts !! 1

main = do
  args <- System.getArgs
  content <- readFile (args !! 0)

  let linesOfFiles = lines content

  let docs = toDocs linesOfFiles
  let corpus = Corpus docs

  let docsAnalysed = Tfidf.search corpus $ args !! 1

  print show length docsAnalysed
  mapM_ print docsAnalysed
