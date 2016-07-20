module TFIDF (tfIdfs) where

-- Represents a Document with id and content
data Document = Document Int String deriving (Show)

-- Represents all documents
data Corpus = Corpus [Document] deriving (Show)

-- Auxiliar functions
countTermsInDoc :: Document -> String -> Int
countTermsInDoc (Document _ "") term = 0
countTermsInDoc _ "" = 0
countTermsInDoc (Document _ content) term = length $ filter (\s -> s == term) docWords
                                            where docWords = words content

docHasTerm :: Document -> String -> Bool
docHasTerm doc term = (countTermsInDoc doc term) > 0

-- TF: Term frequency = 1 + log f / t
--
-- f = number of terms in the document.
-- t = total of terms in the document.
--
tf :: Document -> String -> Float
tf (Document _ "") _ =  0.0
tf (Document _ _) "" =  0.0
tf doc@(Document _ content) term | termInDoc == 0         = 0
                                 | totalOfTermsInDoc == 0 = 0
                                 | otherwise              = 1.0 + (log (termInDoc / totalOfTermsInDoc))
                                 where termInDoc          = fromIntegral $ countTermsInDoc doc term
                                       totalOfTermsInDoc  = fromIntegral $ length $ words content

-- IDF: Inverse document frequency = log ( 1 + t / d).
--
-- t = total of documents in the corpus.
-- d = number of documents containing the term in the corpus.
--
idf :: Corpus -> String -> Float
idf (Corpus []) _ = 0.0
idf _ "" = 0.0
idf (Corpus docs) term = log $ 1 + totalDocsInCorpus / documentsContainingTheTerm
                         where totalDocsInCorpus          = fromIntegral $ length docs
                               documentsContainingTheTerm = fromIntegral $ length $ filter (\d -> docHasTerm d term) docs

-- TF-IDF: Term frequency inverse document frequency = tf * idf
--
tfIdfDocument :: [Document] -> Float -> String -> [(Document, Float)]
tfIdfDocument [] _ _ = []
tfIdfDocument _ _ "" = []
tfIdfDocument (hdoc:tdoc) idfValue term = (hdoc, tfValue * idfValue):rest
                                          where rest    = tfIdfDocument tdoc idfValue term
                                                tfValue = tf hdoc term

tfIdfTerm :: Corpus -> String -> [(Document, Float)]
tfIdfTerm (Corpus []) _ = []
tfIdfTerm _ "" = []
tfIdfTerm corpus@(Corpus docs) term = tfIdfDocument docs vIdf term 
                                   -- First I calculate the idf then pass the value already processed
                                   where vIdf = idf corpus term

d1 = (Document 1 "a a a b b c")
d2 = (Document 2 "a a a b b c x")
d3 = (Document 3 "d d d d d d d d a a")
d4 = (Document 3 "y a")

cp = (Corpus [d1, d2, d3, d4])
