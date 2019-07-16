module Data.SyntaxTree
  ( module Data.SyntaxTree
  , module Data.Tree
  , module Data.ConllToken
  ) where

import           Data.ConllToken
import           Data.Map
import           Data.Tree
import           Protolude

--------------------------------------------------------------------------------

-- | Wrapper for the tree to describe syntax tree
--   as a structure of connected ConllTokens
type SyntaxtTree cpos fpos ger feats lemma =
  Tree (ConllToken cpos fpos ger feats lemma)

-- |
createSyntaxTree :: [ConllToken cpos fpos ger feats lemma]
                 -> Either SyntaxErrorCoNLL (SyntaxtTree cpos fpos ger feats lemma)
createSyntaxTree conllLines =
  note TheresNoRoot $ listToMaybe (treeFrom 0)
    where
      indexToNode = fromList [ (_tnId l,l)
                             | l <- conllLines
                             ]
      headToNode  = fromListWith (++) [ (_tnHead node,[node])
                                      | node <- conllLines
                                      ]

      treeFrom n = let relations = fromMaybe [] $ lookup n headToNode
                   in [ Node { rootLabel = node
                             , subForest = treeFrom (_tnId node)
                             }
                      | node <- relations
                      ]
