

module Data.ParsedSentence ( module Data.ParsedSentence
                           , module Data.SyntaxTree
                           ) where

import           Protolude
import           Data.SyntaxTree
import           Data.Map


data ParsedSentence cpos fpos ger feats lemma = ParsedSentence
       { _rootNode    :: SyntaxtTree cpos fpos ger feats lemma
       , _indexToNode :: Map Int (SyntaxtTree cpos fpos ger feats lemma)
       , _headToNode  :: Map Int [SyntaxtTree cpos fpos ger feats lemma] 
                         -- ^ All relations having as "head" the given index  
       } deriving(Show,Read,Eq,Generic)



sentenceFromRootNode :: SyntaxtTree cpos fpos ger feats lemma
                     -> ParsedSentence cpos fpos ger feats lemma

sentenceFromRootNode _rootNode = ParsedSentence{..}
  where
    _indexToNode = fromList          [ (_tnId   $ rootLabel node, node ) | node <- everyNode]
    
    _headToNode  = fromListWith (++) [ (_tnHead $ rootLabel node,[node]) | node <- everyNode]

    everyNode    = everyNodeFrom _rootNode
    everyNodeFrom node@(Node _ children) = node:( everyNodeFrom =<< children)
