

module Data.SyntaxTree ( module Data.SyntaxTree
                       , module Data.Tree
                       , module Data.ConllToken
                       ) where


import           Data.ConllToken
import           Data.Map
import           Data.Tree
import           Protolude

type SyntaxtTree cpos fpos ger feats lemma = Tree (ConllToken cpos fpos ger feats lemma)

data ParsedSentence cpos fpos ger feats lemma = ParsedSentence
       { _rootNode    :: SyntaxtTree cpos fpos ger feats lemma
       , _indexToNode :: Map Int (SyntaxtTree cpos fpos ger feats lemma)
       , _headToNode  :: Map Int [SyntaxtTree cpos fpos ger feats lemma] 
                         -- ^ All relations having as "head" the given index  
       } deriving(Show,Read,Eq,Generic)


createSyntaxTree :: [ConllToken cpos fpos ger feats lemma] 
                 -> Either SyntaxErrorCoNLL (SyntaxtTree cpos fpos ger feats lemma)

createSyntaxTree conllLines = note TheresNoRoot $ listToMaybe (treeFrom 0)

  where

    indexToNode = fromList [ (_tnId l,l) | l <- conllLines]


    headToNode  = fromListWith (++) [ (_tnHead node,[node]) 
                                    | (i,node) <- reverse $ assocs indexToNode
                                    ] -- ^ we do not need to "reverse", but that would make
                                      --   the result order closer to the original

    

    treeFrom n = let relations = fromMaybe [] $ lookup n headToNode
                            
                  in [ Node{ rootLabel = node
                           , subForest = treeFrom (_tnId node)
                           }
                     | node <- relations
                     ]

