{-# LANGUAGE DeriveAnyClass #-}


module Model.UniversalTreebank where 




import           Protolude
import           Data.TagLabel

-- TODO: are these the actual model from the universal treebank ?
--       maybe we'd need several different models if they are not
--       the same.

data POS = CC
         | CD
         | Colon       (SpelledAs ":"    )
         | DT
         | EX
         | IN
         | JJ
         | JJR
         | LRB_        (SpelledAs "-LRB-")
         | LS
         | MD
         | NN
         | NNP
         | NNPS
         | NNS
         | POS
         | PRP
         | PRP_Dollar  (SpelledAs "PRP$")
         | Punctuation (SpelledAs ".") -- ^ Not just dots, at least it also includes question marks
         | Quotes      (SpelledAs "''")
         | Quotes2     (SpelledAs "``")
         | RB
         | RBR
         | RP
         | RRB_        (SpelledAs "-RRB-")
         | Semicolon   (SpelledAs ",")
         | TO
         | VB
         | VBD
         | VBG
         | VBN
         | VBP
         | VBZ
         | WDT
         | WP_Dollar     (SpelledAs "WP$")
         | WRB
         deriving(Show,Read,Eq,Ord,Generic,TagLabel)

data REL = Acl
         | ACL_RELCL     (SpelledAs "acl:relcl"   )
         | Advcl
         | Advmod
         | Amod
         | Appos
         | Aux
         | Auxpass
         | Case
         | Cc
         | Cc_preconj    (SpelledAs "cc:preconj"  )
         | Ccomp
         | Compound
         | Compound_PRT  (SpelledAs "compound:prt")
         | Conj
         | Cop
         | Csubj
         | Dep
         | Det
         | Dobj
         | Expl
         | Iobj
         | Mark
         | Mwe
         | Neg
         | Nmod
         | Nmod_npmod    (SpelledAs "nmod:npmod"  )
         | NMOD_POS      (SpelledAs "nmod:poss"   )
         | NMOD_TMOD     (SpelledAs "nmod:tmod"   )
         | Nsubj
         | Nsubjpass
         | Nummod
         | Parataxis
         | Punct
         | ROOT
         | Xcomp
         deriving(Show,Read,Eq,Ord,Generic,TagLabel)


