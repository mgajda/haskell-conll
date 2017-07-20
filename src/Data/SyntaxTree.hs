

module Data.SyntaxTree ( module Data.SyntaxTree
	                   , module Data.Tree
	                   ) where


import           Protolude
import           Data.ConllToken
import           Data.Tree


type SyntaxtTree cpos fpos ger feats lemma = Tree (Token cpos fpos ger feats lemma)

data ParsedSentence cpos fpos ger feats lemma = ParsedSentence
       { _rootNode    :: SyntaxtTree cpos fpos ger feats lemma
       , _indexToNode :: Map Int (SyntaxtTree cpos fpos ger feats lemma)
       , _headToNode  :: Map Int [SyntaxtTree cpos fpos ger feats lemma] 
                         -- ^ All relations having as "head" the given index  
       } deriving(Show,Read,Eq,Generic,Ord)








