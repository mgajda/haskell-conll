{-# LANGUAGE DeriveAnyClass #-}


module Model.PennTreebank where 



import           Protolude
import           Data.TagLabel


-------------------------------------------------
-- | (Penn) Treebank Tag-set 
--  For documentation see http://www.comp.leeds.ac.uk/amalgam/tagsets/upenn.html
--
data POS = CC
         | CD
         | ClosePar    (SpelledAs ")"    )
         | Colon       (SpelledAs ":"    )
         | Coma        (SpelledAs ",")
         | Dash        (SpelledAs "--"   ) 
         | Dollar      (SpelledAs "$"    )
         | DT
         | EX
         | FW
         | IN
         | JJ
         | JJR
         | JJS
         | LRB_        (SpelledAs "-LRB-")
         | LS
         | MD
         | NN
         | NNP
         | NNPS
         | NNS
         | OpenPar     (SpelledAs "("    ) 
         | PDT
         | POS
         | PRP
         | PRP_Dollar  (SpelledAs "PRP$")
         | Quotes      (SpelledAs "''")
         | Quotes2     (SpelledAs "``")
         | RB
         | RBR
         | RBS
         | RP
         | RRB_        (SpelledAs "-RRB-")
         | SYM
         | Terminator  (SpelledAs "."    )
         | TO
         | UH
         | VB
         | VBD
         | VBG
         | VBN
         | VBP
         | VBZ
         | WDT
         | WP
         | WP_Dollar     (SpelledAs "WP$")
         | WRB
         deriving(Show,Read,Eq,Ord,Generic,TagLabel)





