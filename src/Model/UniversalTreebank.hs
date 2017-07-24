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


---------------------------------------------
-- | Tags from Universal Dependencies project
--   see `http://universaldependencies.org/en/dep/`


data REL = Acl                                    -- ^ clausal modifier of noun
         | Acl_Relcl  (SpelledAs "acl:relcl"   )  -- ^ relative clause modifier
         | Advcl                                  -- ^ adverbial clause modifier
         | Advmod                                 -- ^ adverbial modifier
         | Amod                                   -- ^ adjectival modifier
         | Appos                                  -- ^ appositional modifier
         | Aux                                    -- ^ auxiliary
         | Auxpass                                -- ^ passive auxiliary
         | Case                                   -- ^ case marking
         | Cc                                     -- ^ coordination
         | Cc_Preconj (SpelledAs "cc:preconj"  )  -- ^ preconjunct
         | Ccomp                                  -- ^ clausal complement
         | Compound                               -- ^ compound
         | Compound_Pr(SpelledAs "compound:prt")  -- ^ phrasal verb particle
         | Conj                                   -- ^ conjunct
         | Cop                                    -- ^ copula
         | Csubj                                  -- ^ clausal subject
         | Csubjpass                              -- ^ clausal passive subject
         | Dep                                    -- ^ dependent
         | Det                                    -- ^ determiner
         | Det_Predet (SpelledAs "det:predet"  )  -- ^ predeterminer
         | Discourse                              -- ^ discourse element
         | Dislocated                             -- ^ dislocated elements
         | Dobj                                   -- ^ direct object
         | Expl                                   -- ^ expletive
         | Fixed      (SpelledAs "mwe")           -- ^ multi-word expression
         | Flat                                   -- ^ name
         | Foreign                                -- ^ foreign words
         | Goeswith                               -- ^ goes with
         | Iobj                                   -- ^ indirect object
         | List                                   -- ^ list
         | Mark                                   -- ^ marker
         | Neg                                    -- ^ negation modifier
         | Nmod                                   -- ^ nominal modifier
         | Nmod_npmod (SpelledAs "nmod:npmod"  )  -- ^ noun phrase as adverbial modifier
         | Nmod_poss  (SpelledAs "nmod:poss"   )  -- ^ possessive nominal modifier
         | Nmod_tmod  (SpelledAs "nmod:tmod"   )  -- ^ temporal modifier
         | Nsubj                                  -- ^ nominal subject
         | Nsubjpass                              -- ^ passive nominal subject
         | Nummod                                 -- ^ numeric modifier
         | Orphan                                 -- ^ remnant in ellipsis
         | Parataxis                              -- ^ parataxis
         | Punct                                  -- ^ punctuation
         | Reparandum                             -- ^ overridden disfluency
         | ROOT                                   -- ^ root
         | Vocative                               -- ^ vocative
         | Xcomp                                  -- ^ open clausal complement  
         deriving(Show,Read,Eq,Ord,Generic,TagLabel)

