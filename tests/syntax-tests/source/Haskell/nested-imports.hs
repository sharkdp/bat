module Nested (NonEmpty ((:|)), last, Tree (Leaf, Branch), (+)) where

import Data.List.NonEmpty (NonEmpty ((:|)), last)
import Data.List.NonEmpty (NonEmpty ((:|)), head, last)
import Data.Tree (Tree (Node), flatten)
import Data.List (foldl, (++))
import qualified Data.Map as Map

value = last [1, 2, 3]
