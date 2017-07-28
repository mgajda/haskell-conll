{-# LANGUAGE DeriveAnyClass #-}

module Model.NerCoreNlp where 

import           Protolude
import           Data.TagLabel


{- | Named Entity Recognition (NER) tag set used by `CoreNLP`.

     TODO: find documentation where this list is elicited.
-}


-- | CoreNLP's NER tag set:

data NER = O
         | CARDINAL 
         | DATE
         | DURATION
         | FACILITY 
         | GPE
         | LOCATION 
         | MEASURE
         | MISC
         | MONEY
         | NUMBER
         | ORDINAL
         | ORGANIZATION 
         | PERCENT 
         | PERSON 
         | SET
         | TIME
         deriving(Show,Eq,Read,Ord,Generic,TagLabel)



