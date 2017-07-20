

module Data.ConllToken where


import           Protolude
import           Control.Lens


data ConllToken cpos fpos ger feats lemma = ConllToken
    { _tnId        :: Int     -- ^ Index number
    , _tnWord      :: Text    -- ^ Parsed word or punctuation symbol
    , _tnLemma     :: lemma   -- ^ Lemma or stem
    , _tnPosCG     :: cpos    -- ^ Part-of-Speech (POS) coarse-grained (PRON, VERB, DET, NOUN, etc) 
    , _tnPosFG     :: fpos    -- ^ Part-of-Speech (POS) fine-grained   (PRP, VBD, DT, NN etc.) 
    , _tnFeats     :: feats   -- ^ Unordered set of syntactic and/or morphological features.
    , _tnHead      :: Int     -- ^ Head of the current token, which is either a value of ID or '0'.
    , _tnRel       :: ger     -- ^ grammatical relationships between different words in the sentence, alined with Head
    , _tnHeadProj  :: Text    -- ^ Projective head of current token.
    , _tnRelProj   :: Text    -- ^ Dependency relation to the PHEAD.     
    } deriving (Show, Read, Eq, Ord, Generic, Functor)



-- | SyntaxErrorCoNLL:  Reason                      LineNumber Culprit
--     |                 |                             |         |
--     v                 v                             v         v
data SyntaxErrorCoNLL = UnkonwnPosTag                 Int       Text 
                      | UnkwownRelTag                 Int       Text 
                      | CoulNotParseInteger           Int       Text 
                      | InvalidNumberOfElementsOnLine Int       Text 
                      | TheresNoRoot
                      deriving(Show,Read,Eq,Ord)


$(makeLenses ''ConllToken      )
$(makePrisms ''SyntaxErrorCoNLL)


