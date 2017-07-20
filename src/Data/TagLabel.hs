

module Data.TagLabel ( module GHC.Generics
                     , TagLabel(..)
                     , SpelledAs(..)
                     , fromLabelText
                     , toLabelText
                     ) where



import           Data.Char
import           Data.Map
import           GHC.Generics    (Generic,Rep(..),(:+:)(..))
import           GHC.TypeLits
import           Protolude       hiding(fromLabel)
import qualified Data.Text       as T
import qualified GHC.Generics    as G

class (Ord label) => TagLabel label where
   labelMap ::  Map Text label

   default labelMap :: (Generic label, GLabel (Rep label) ) => Map Text label
   labelMap = fmap G.to . fromList $  gLabelMap (Proxy :: Proxy (Rep label))

   reverseLabelMap :: Map label Text
   reverseLabelMap = fromList [ (b,a) | (a,b) <- assocs labelMap]


fromLabelText :: (TagLabel label) => Text -> Maybe label
fromLabelText t = lookup t labelMap

toLabelText   :: (TagLabel label) => label -> Text
toLabelText l =  fromMaybe (error "imposible branch on Data.Label") 
              $  lookup l reverseLabelMap


{- | The class `TagLabel` represents types to be used as labels for CoreNLP, such POS sets or gramatical relations.

     The class can be manually derived for special cases, but it is expected to be auto derived in most cases.

     First to enable autoderive, set:
     
     ```
     { -# LANGUAGE DeriveAnyClass #- }
     ```

     now you can use it on any enumeration already deriving `Ord` and `Generic`, for example defining:
     ```
     data Foo = Foo1
              | Foo2
              | Foo3
              deriving(Ord,Eq,Generic,TagLabel)
     ```
     would make the type `Foo` to hold the labels `"Foo1"`, `"Foo2"`, `"Foo3"`.

     It is also posible to derive `TagLabel` on _tree like_ types, for example defining:
     ```
     data Xoo = Xoo1
              | Xoo2
              | Xoo3
              deriving(Ord,Eq,Generic,TagLabel)

     data Moo = Moo1
              | Moo2
              | Moo3
              deriving(Ord,Eq,Generic,TagLabel)
     
     data Complex = Comp1
                  | Comp2
                  | AnyFoo Foo
                  | AnyXooOrMoo (Either Xoo Moo)
                  | Comp3
                  deriving(Ord,Eq,Generic,TagLabel)
     ```
     would make the type `Complex` to hold the labels:
     . `"Comp1"`
     . `"Comp2"`
     - `"Foo1"`
     - `"Foo2"`
     - `"Foo3"`
     - `"Xoo1"`
     - `"Xoo2"`
     - `"Xoo3"`
     - `"Moo1"`
     - `"Moo2"`
     - `"Moo3"`
     - `"Comp3"`

     Notice all the _tree fingers_ appear as labels, but constructors `AnyFoo` or `AnyXooOrMoo` __do not__ .

     It is specially handy use `Either` as a sum of labels, forexample we can define
     ```
     type Adjective = Either AdjComparative AdjSuperlative
     ```

     To define custom irregular labels, use `SpelledAs`, for example:
     ```
     data Dollar = Dollar (SpelledAs "$") deriving(Ord,Eq,Generic,TagLabel)
     ```
     The `Dollar` type would have the label as "$".


     If one try to define `TagLabel` over a type where it makes no sense, the compiler will complain and __will not allow__ it.
     Some example of types that can not derive `TagLabel`:

     ```
     data Foo = Foo1
              | FooN Int    -- `Int` can not be treat as a set of labels, hence `Foo` can't either

     data Zoo = Zoo Xoo Moo -- Only singletons or sum types can derive `TagLabel`, here `Zoo` is a product
                            -- type

     data EmptyData         -- Empty types (Types without constructors, like `Void`) can not be derived
                            -- as `TagLabels`.

     data Woo = Woo Woo     -- WARNING: recursive and mutually recursive types __are able__ to derive `TagLabel`,
                            -- but they __will fail__ at run time if treated as such.
     ```


-}


--------------------------------------------------------------------
--------------------------------------------------------------------
-- Private "default class" machinery:


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
-- Standard labels:

-- | The labels of a sum type, is the cartesian sum of its labels 
instance (TagLabel l1, TagLabel l2)  => TagLabel (Either l1 l2)  

-- | The label of a `SpelledAs x` is x, where x is a type level literal string (a "Symbol").
instance (KnownSymbol symbol) => TagLabel (SpelledAs symbol) where
   labelMap = fromList [(toSL $ symbolVal (Proxy :: Proxy symbol),  SymbolProxy)]



data SpelledAs (s :: Symbol) = SymbolProxy deriving(Show,Read,Eq,Ord,Generic)


