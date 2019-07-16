{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.TagLabel
        ( module GHC.Generics
        , TagLabel(..)
        , SpelledAs(..)
        , fromLabelText
        , toLabelText
        ) where

import           Data.Char
import           Data.Map
import           GHC.Generics    (Generic,Rep(..),(:+:)(..))
import           GHC.TypeLits
import           Protolude       hiding (fromLabel)
import           Protolude.Error
import qualified Data.Text       as T
import qualified GHC.Generics    as G

--------------------------------------------------------------------------------

-- |
class (Ord label) => TagLabel label where
   labelMap ::  Map Text label

   default labelMap :: (Generic label, GLabel (Rep label) ) => Map Text label
   labelMap = fmap G.to . fromList $  gLabelMap (Proxy :: Proxy (Rep label))

   reverseLabelMap :: Map label Text
   reverseLabelMap = fromList [ (b,a) | (a,b) <- assocs labelMap]

-- |
fromLabelText :: (TagLabel label) => Text -> Maybe label
fromLabelText t = lookup t labelMap

-- |
toLabelText   :: (TagLabel label) => label -> Text
toLabelText l =
  fromMaybe (error "imposible branch on Data.Label")
    $ lookup l reverseLabelMap

--------------------------------------------------------------------------------
-- Machinery

class GLabel f where
    gLabelMap :: Proxy f -> [(Text, f a)]

-- | Definitions: `D1` (instance base on to type definition metadata...we can ignore it and focus on `f`)
instance (GLabel f) => GLabel (D1 meta f) where
    gLabelMap _ = second M1 <$> gLabelMap (Proxy :: Proxy f)


-- | The labels of sum types are the labels of its parts
instance (GLabel f, GLabel g) => GLabel ( f :+: g) where
  gLabelMap _ =  ( second L1  <$> gLabelMap (Proxy :: Proxy f))
              ++ ( second R1  <$> gLabelMap (Proxy :: Proxy g))

-- | The label of a constructor without arguments is the constructor's name itself
instance (KnownSymbol constructor) => GLabel (C1 ('MetaCons constructor a b) U1) where
  gLabelMap _ = [( convert . toSL $ symbolVal (Proxy :: Proxy constructor) , M1 U1)]
    where
      convert s = if T.any isLower s then T.toLower s
                                     else s
-- | The labels with a constructor with a single argument, are the labels of its argument
instance (TagLabel labelType) => GLabel (C1 meta1 (S1 meta2 (K1 meta3 labelType))) where
  gLabelMap _ = assocs $ (M1 . M1 . K1)  <$> labelMap

---------------------------------------------------------------------------------------
-- Standard labels

-- | The labels of a sum type, is the cartesian sum of its labels
instance (TagLabel l1, TagLabel l2)  => TagLabel (Either l1 l2)

-- | The label of a `SpelledAs x` is x, where x is a type level literal string (a "Symbol").
instance (KnownSymbol symbol) => TagLabel (SpelledAs symbol) where
   labelMap = fromList [(toSL $ symbolVal (Proxy :: Proxy symbol),  SymbolProxy)]

data SpelledAs (s :: Symbol) =
  SymbolProxy
    deriving(Show, Read, Eq, Ord, Generic)
