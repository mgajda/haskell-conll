{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.UniversalTreebank where

import           Data.TagLabel
import           Protolude

--------------------------------------------------------------------------------

-- | Tags from Universal Dependencies project
--   see `http://universaldependencies.org/en/dep/`
data REL =
  Acl                                    -- ^ clausal modifier of noun
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
  | Root                                   -- ^ root (not main)
  | Vocative                               -- ^ vocative
  | Xcomp                                  -- ^ open clausal complement
  | Unknown                                -- ^ unknown
  deriving(Show, Read, Eq, Ord, Generic, TagLabel)
