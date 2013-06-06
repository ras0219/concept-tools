module Concepts (Concept(..),
                 TypeExpr(Type),
                 commonType, valueType, resultType,
                 satisfies,
                 simplify,
                 showConcepts,
                 simplify') where

import Data.List hiding (union)
import Data.Set (Set, singleton, union, isSubsetOf, fold,
                 empty, fromList, toList)

-- |Checks if the first concept set satisfies the constraints of the second.
-- Will fully simplify both arguments
satisfies :: Set Concept -> Set Concept -> Bool

-- |Checks if the first concept set satisfies the constraints of the second.
-- Will fully simplify both arguments
simplify :: Set Concept -> Set Concept

-- |Checks if the first concept set satisfies the constraints of the second.
-- Will fully simplify both arguments
showConcepts :: Set Concept -> String

-- |Checks if the first concept set satisfies the constraints of the second.
-- Will fully simplify both arguments
simplify' :: Concept -> [Concept]

data TypeExpr = Type String
              | Scope TypeExpr String
              | Ref TypeExpr
              | Const TypeExpr
              -- Type Functions
              | TypeFunc String [TypeExpr]
              | Binary TypeExpr String TypeExpr
              | Unary String TypeExpr
              | App String [TypeExpr]
                deriving (Eq, Ord)

instance Show TypeExpr where
    show (Type s) = s
    show (Scope t s) = show t ++ "::" ++ s
    show (Ref t) = show t ++ "&"
    show (Const t) = "const " ++ show t
    show (TypeFunc s ts) = s ++ "<" ++ intercalate ", " (map show ts) ++ ">"
    show (Binary t1 s t2) = show t1 ++ " " ++ s ++ " " ++ show t2
    show (Unary s t) = s ++ show t
    show (App s ts) = s ++ "(" ++ intercalate ", " (map show ts) ++ ")"

commonType a b = t
    where (a',b') = if a < b then (a,b) else (b,a)
          t = if a == b
              then a
              else TypeFunc "CommonType" [a', b']

valueType a = TypeFunc "ValueType" [a]
resultType a bs = TypeFunc "ResultType" (a:bs)
constRef t = Const (Ref t)

data Concept = Valid TypeExpr
             ---- Language Concepts
             | Same TypeExpr TypeExpr
             | Derived TypeExpr TypeExpr
             | Convertible TypeExpr TypeExpr
             | Common TypeExpr TypeExpr
             | Integral TypeExpr
             | SignedIntegral TypeExpr
             | UnsignedIntegral TypeExpr
             ---- Foundational Concepts
             | EqualityComparable TypeExpr
             | EqualityComparable2 TypeExpr TypeExpr
             | WeaklyOrdered TypeExpr
             | WeaklyOrdered2 TypeExpr TypeExpr
             | TotallyOrdered TypeExpr
             | TotallyOrdered2 TypeExpr TypeExpr
             | Semiregular TypeExpr
             | Regular TypeExpr
             ---- Function Concepts
             | Function TypeExpr [TypeExpr]
             | RegularFunction TypeExpr [TypeExpr]

             | Predicate TypeExpr [TypeExpr]
             | Relation TypeExpr TypeExpr
             | Relation2 TypeExpr TypeExpr TypeExpr

             | UnaryOperation TypeExpr TypeExpr
             | BinaryOperation TypeExpr TypeExpr
             | BinaryOperation2 TypeExpr TypeExpr TypeExpr
             ---- Iterator Concepts
             -- Iterator Properties
             | Readable TypeExpr
             | MoveWritable TypeExpr TypeExpr
             | Writable TypeExpr TypeExpr
             | IndirectlyMovable TypeExpr TypeExpr
             | IndirectlyCopyable TypeExpr TypeExpr
             -- Incrementable Types
             | WeaklyIncrementable TypeExpr
             | Incrementable TypeExpr
             | Decrementable TypeExpr -- my added
             | Advancable TypeExpr -- my added
             -- Iterator Abstractions
             | WeakInputIterator TypeExpr
             | InputIterator TypeExpr
             | ForwardIterator TypeExpr
             | BidirectionalIterator TypeExpr
             | RandomAccessIterator TypeExpr
             ---- Rearrangements
             | Permutable TypeExpr
             | Mergeable TypeExpr TypeExpr TypeExpr
             | Mergeable2 TypeExpr TypeExpr TypeExpr TypeExpr
             | Sortable TypeExpr
             | Sortable2 TypeExpr TypeExpr
               deriving (Show, Eq, Ord)

bool = Type "bool"

-- Some guards
simplify' c@(Valid (TypeFunc "CommonType" [a, b]))
    | a == b = []
    | otherwise = [c]
-- C++ Standard(ish)
simplify' (EqualityComparable2 t u) = [ Common t u
                                     , EqualityComparable (commonType t u)
                                     , Convertible (Binary t "==" u) bool
                                     , Convertible (Binary u "==" t) bool
                                     , Convertible (Binary t "!=" u) bool
                                     , Convertible (Binary u "!=" t) bool ]
simplify' (Regular i) = [ Regular i
                       , Semiregular i
                       , EqualityComparable i ]
simplify' (TotallyOrdered t) = [ EqualityComparable t
                              , WeaklyOrdered t ]
simplify' (TotallyOrdered2 t u) = [ EqualityComparable2 t u
                                 , TotallyOrdered (commonType t u)
                                 , WeaklyOrdered2 t u ]
-- functions
simplify' (RegularFunction p l) = [ RegularFunction p l
                                 , Function p l ]
simplify' (Predicate p l) = [ RegularFunction p l,
                             Convertible (resultType p l) bool ]
simplify' (Relation r t) = [ Predicate r [t, t] ]
simplify' (Relation2 r t u) = [ Relation r t
                             , Relation r u
                             , Common t u
                             , Relation r (commonType t u)
                             , Convertible (resultType r [t, u]) bool
                             , Convertible (resultType r [u, t]) bool ]
-- operations
simplify' (UnaryOperation o t) = [ Function o [t]
                                , Convertible (resultType o [t]) t ]
simplify' (BinaryOperation o t) = [ Function o [t,t]
                                 , Convertible (resultType o [t,t]) t ]
simplify' (BinaryOperation2 o t u) = [ BinaryOperation o t
                                    , BinaryOperation o u
                                    , Common t u
                                    , BinaryOperation o c
                                    , Convertible (resultType o [t,u]) c
                                    , Convertible (resultType o [u,t]) c ]
    where c = commonType t u
-- iterators
simplify' (Readable i) = [ Semiregular i
                        , Valid (valueType i)
                        , Readable i ]
simplify' (MoveWritable t o) = [ Semiregular o, MoveWritable t o ]
simplify' (Writable t o) = [ Semiregular o, MoveWritable t o, Writable t o ]
simplify' (IndirectlyMovable i o) = [ Readable i
                                   , Semiregular o
                                   , MoveWritable (valueType i) o ]

simplify' (IndirectlyCopyable i o) = [ Readable i
                                    , Semiregular o
                                    , Writable (valueType i) o ]
-- incrementables
simplify' (WeaklyIncrementable i) = [ Semiregular i
                                   , WeaklyIncrementable i ]
simplify' (Incrementable i) = [ Regular i
                             , WeaklyIncrementable i
                             , Incrementable i ]
-- iterator types
simplify' (WeakInputIterator i) = [ WeaklyIncrementable i
                             , Readable i ]
simplify' (InputIterator i) = [ WeakInputIterator i
                         , EqualityComparable i ]
simplify' (ForwardIterator i) = [ InputIterator i
                           , Incrementable i ]
simplify' (BidirectionalIterator i) = [ ForwardIterator i
                                 , Decrementable i ]
simplify' (RandomAccessIterator i) = [ BidirectionalIterator i
                                , TotallyOrdered i
                                , Advancable i ]
-- rearrangements
simplify' (Permutable i) = [ ForwardIterator i
                          , Semiregular (valueType i)
                          , IndirectlyMovable i i ]
simplify' (Mergeable i j o) = [ InputIterator i
                             , InputIterator j
                             , WeaklyIncrementable o
                             , TotallyOrdered2 (valueType i) (valueType j)
                             , IndirectlyCopyable i o
                             , IndirectlyCopyable j o ]
simplify' (Mergeable2 i j o r) = [ InputIterator i
                                , InputIterator j
                                , WeaklyIncrementable o
                                , Relation2 r (valueType i) (valueType j)
                                , IndirectlyCopyable i o ]
simplify' (Sortable i) = [ ForwardIterator i
                        , TotallyOrdered (valueType i)
                        , Permutable i ]
simplify' (Sortable2 i r) = [ ForwardIterator i
                           , Relation r (valueType i)
                           , Permutable i ]
-- My stuff
simplify' c = [c]


satisfies s t = isSubsetOf (simplify t) (simplify s)

bind :: (Ord a, Ord b) => Set a -> (a -> Set b) -> Set b
bind m f = fold (\n -> union (f n)) empty m

simplify s = s''
    where s' = s `bind` (\c -> fromList $ simplify' c)
          s'' = if s == s'
                then s
                else simplify s'

--------------------
showConcepts cs = intercalate "\n" $ map showConcept $ toList cs

showConcept (Convertible t1 t2) =
    "Convertible{" ++ show t1 ++ ", " ++ show t2 ++ "}"
showConcept c = show c