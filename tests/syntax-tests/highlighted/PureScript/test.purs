[38;2;117;113;94m-- | This module defines a datatype `Pair` together with a few useful instances[0m
[38;2;117;113;94m-- | and helper functions. Note that this is not just `Tuple a a` but rather a[0m
[38;2;117;113;94m-- | list with exactly two elements. Specifically, the `Functor` instance maps[0m
[38;2;117;113;94m-- | over both values (in contrast to the `Functor` instance for `Tuple a`).[0m
[38;2;249;38;114mmodule[0m[38;2;248;248;242m [0m[38;2;248;248;242mData.Pair[0m
[38;2;248;248;242m  ( [0m[38;2;248;248;242mPair[0m[38;2;248;248;242m(..)[0m
[38;2;248;248;242m  , (~)[0m
[38;2;248;248;242m  , [0m[38;2;248;248;242mfst[0m
[38;2;248;248;242m  , [0m[38;2;248;248;242msnd[0m
[38;2;248;248;242m  , [0m[38;2;248;248;242mcurry[0m
[38;2;248;248;242m  , [0m[38;2;248;248;242muncurry[0m
[38;2;248;248;242m  , [0m[38;2;248;248;242mswap[0m
[38;2;248;248;242m  ) [0m[38;2;249;38;114mwhere[0m

[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;248;248;242mPrelude[0m

[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;248;248;242mData.Foldable[0m[38;2;248;248;242m (class [0m[38;2;166;226;46mFoldable[0m[38;2;248;248;242m)[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;248;248;242mData.Traversable[0m[38;2;248;248;242m (class [0m[38;2;166;226;46mTraversable[0m[38;2;248;248;242m)[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;248;248;242mData.Distributive[0m[38;2;248;248;242m (class [0m[38;2;166;226;46mDistributive[0m[38;2;248;248;242m)[0m

[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;248;248;242mTest.QuickCheck.Arbitrary[0m[38;2;248;248;242m (class [0m[38;2;166;226;46mArbitrary[0m[38;2;248;248;242m, [0m[38;2;166;226;46marbitrary[0m[38;2;248;248;242m)[0m

[38;2;117;113;94m-- | A pair simply consists of two values of the same type.[0m
[38;2;249;38;114mdata[0m[38;2;248;248;242m [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m a [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m a a[0m

[38;2;249;38;114minfixl[0m[38;2;248;248;242m [0m[38;2;190;132;255m6[0m[38;2;248;248;242m [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114mas[0m[38;2;248;248;242m [0m[38;2;249;38;114m~[0m

[38;2;117;113;94m-- | Returns the first component of a pair.[0m
[38;2;248;248;242mfst ∷ ∀ a[0m[38;2;249;38;114m.[0m[38;2;248;248;242m [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m a → a[0m
[38;2;248;248;242mfst (x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m _) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m x[0m

[38;2;117;113;94m-- | Returns the second component of a pair.[0m
[38;2;248;248;242msnd ∷ ∀ a[0m[38;2;249;38;114m.[0m[38;2;248;248;242m [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m a → a[0m
[38;2;248;248;242msnd (_ [0m[38;2;249;38;114m~[0m[38;2;248;248;242m y) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m y[0m

[38;2;117;113;94m-- | Turn a function that expects a pair into a function of two arguments.[0m
[38;2;102;217;239mcurry[0m[38;2;248;248;242m ∷ ∀ a b[0m[38;2;249;38;114m.[0m[38;2;248;248;242m ([0m[38;2;190;132;255mPair[0m[38;2;248;248;242m a → b) → a → a → b[0m
[38;2;102;217;239mcurry[0m[38;2;248;248;242m f x y [0m[38;2;249;38;114m=[0m[38;2;248;248;242m f (x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m y)[0m

[38;2;117;113;94m-- | Turn a function of two arguments into a function that expects a pair.[0m
[38;2;102;217;239muncurry[0m[38;2;248;248;242m ∷ ∀ a b[0m[38;2;249;38;114m.[0m[38;2;248;248;242m (a → a → b) → [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m a → b[0m
[38;2;102;217;239muncurry[0m[38;2;248;248;242m f (x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m y) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m f x y[0m

[38;2;117;113;94m-- | Exchange the two components of the pair[0m
[38;2;248;248;242mswap ∷ ∀ a[0m[38;2;249;38;114m.[0m[38;2;248;248;242m [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m a → [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m a[0m
[38;2;248;248;242mswap (x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m y) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m y [0m[38;2;249;38;114m~[0m[38;2;248;248;242m x[0m

[38;2;248;248;242mderive [0m[38;2;249;38;114minstance[0m[38;2;248;248;242m eqPair ∷ [0m[38;2;190;132;255mEq[0m[38;2;248;248;242m a ⇒ [0m[38;2;190;132;255mEq[0m[38;2;248;248;242m ([0m[38;2;190;132;255mPair[0m[38;2;248;248;242m a)[0m

[38;2;248;248;242mderive [0m[38;2;249;38;114minstance[0m[38;2;248;248;242m ordPair ∷ [0m[38;2;190;132;255mOrd[0m[38;2;248;248;242m a ⇒ [0m[38;2;190;132;255mOrd[0m[38;2;248;248;242m ([0m[38;2;190;132;255mPair[0m[38;2;248;248;242m a)[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255mshowPair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mShow[0m[38;2;248;248;242m [0m[38;2;255;255;255ma[0m[38;2;248;248;242m ⇒ [0m[3;38;2;102;217;239mShow[0m[38;2;248;248;242m ([0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;255;255;255ma[0m[38;2;248;248;242m) [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  [0m[38;2;102;217;239mshow[0m[38;2;248;248;242m (x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m y) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116m([0m[38;2;230;219;116m"[0m[38;2;248;248;242m [0m[38;2;249;38;114m<>[0m[38;2;248;248;242m [0m[38;2;102;217;239mshow[0m[38;2;248;248;242m x [0m[38;2;249;38;114m<>[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116m ~ [0m[38;2;230;219;116m"[0m[38;2;248;248;242m [0m[38;2;249;38;114m<>[0m[38;2;248;248;242m [0m[38;2;102;217;239mshow[0m[38;2;248;248;242m y [0m[38;2;249;38;114m<>[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116m)[0m[38;2;230;219;116m"[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255mfunctorPair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mFunctor[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  map f (x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m y) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m f x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m f y[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255mapplyPair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mApply[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  apply (f [0m[38;2;249;38;114m~[0m[38;2;248;248;242m g) (x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m y) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m f x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m g y[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255mapplicativePair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mApplicative[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  [0m[38;2;102;217;239mpure[0m[38;2;248;248;242m x [0m[38;2;249;38;114m=[0m[38;2;248;248;242m x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m x[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255mbindPair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mBind[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  bind (x [0m[38;2;249;38;114m~[0m[38;2;248;248;242m y) f [0m[38;2;249;38;114m=[0m[38;2;248;248;242m fst (f x) [0m[38;2;249;38;114m~[0m[38;2;248;248;242m snd (f y)[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255mmonadPair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mMonad[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mPair[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255msemigroupPair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mSemigroup[0m[38;2;248;248;242m [0m[38;2;255;255;255ma[0m[38;2;248;248;242m ⇒ [0m[3;38;2;102;217;239mSemigroup[0m[38;2;248;248;242m ([0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;255;255;255ma[0m[38;2;248;248;242m) [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  append (x1 [0m[38;2;249;38;114m~[0m[38;2;248;248;242m y1) (x2 [0m[38;2;249;38;114m~[0m[38;2;248;248;242m y2) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m (x1 [0m[38;2;249;38;114m<>[0m[38;2;248;248;242m x2) [0m[38;2;249;38;114m~[0m[38;2;248;248;242m (y1 [0m[38;2;249;38;114m<>[0m[38;2;248;248;242m y2)[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255mmonoidPair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mMonoid[0m[38;2;248;248;242m [0m[38;2;255;255;255ma[0m[38;2;248;248;242m ⇒ [0m[3;38;2;102;217;239mMonoid[0m[38;2;248;248;242m ([0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;255;255;255ma[0m[38;2;248;248;242m) [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  [0m[38;2;102;217;239mmempty[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;102;217;239mmempty[0m[38;2;248;248;242m [0m[38;2;249;38;114m~[0m[38;2;248;248;242m [0m[38;2;102;217;239mmempty[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255mfoldablePair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mFoldable[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  foldr f z ([0m[38;2;190;132;255mPair[0m[38;2;248;248;242m x y) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m x `f` (y `f` z)[0m
[38;2;248;248;242m  foldl f z ([0m[38;2;190;132;255mPair[0m[38;2;248;248;242m x y) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m (z `f` x) `f` y[0m
[38;2;248;248;242m  foldMap f ([0m[38;2;190;132;255mPair[0m[38;2;248;248;242m x y) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m f x [0m[38;2;249;38;114m<>[0m[38;2;248;248;242m f y[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255mtraversablePair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mTraversable[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  traverse f ([0m[38;2;190;132;255mPair[0m[38;2;248;248;242m x y) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114m<$>[0m[38;2;248;248;242m f x [0m[38;2;249;38;114m<[0m[38;2;248;248;242m*[0m[38;2;249;38;114m>[0m[38;2;248;248;242m f y[0m
[38;2;248;248;242m  [0m[38;2;102;217;239msequence[0m[38;2;248;248;242m ([0m[38;2;190;132;255mPair[0m[38;2;248;248;242m mx my) [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114m<$>[0m[38;2;248;248;242m mx [0m[38;2;249;38;114m<[0m[38;2;248;248;242m*[0m[38;2;249;38;114m>[0m[38;2;248;248;242m my[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255mdistributivePair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mDistributive[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  distribute xs [0m[38;2;249;38;114m=[0m[38;2;248;248;242m map fst xs [0m[38;2;249;38;114m~[0m[38;2;248;248;242m map snd xs[0m
[38;2;248;248;242m  collect f xs [0m[38;2;249;38;114m=[0m[38;2;248;248;242m map (fst [0m[38;2;249;38;114m<<<[0m[38;2;248;248;242m f) xs [0m[38;2;249;38;114m~[0m[38;2;248;248;242m map (snd [0m[38;2;249;38;114m<<<[0m[38;2;248;248;242m f) xs[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;255;255;255marbitraryPair[0m[38;2;248;248;242m ∷ [0m[3;38;2;102;217;239mArbitrary[0m[38;2;248;248;242m [0m[38;2;255;255;255ma[0m[38;2;248;248;242m ⇒ [0m[3;38;2;102;217;239mArbitrary[0m[38;2;248;248;242m ([0m[3;38;2;102;217;239mPair[0m[38;2;248;248;242m [0m[38;2;255;255;255ma[0m[38;2;248;248;242m) [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  arbitrary [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mPair[0m[38;2;248;248;242m [0m[38;2;249;38;114m<$>[0m[38;2;248;248;242m arbitrary [0m[38;2;249;38;114m<[0m[38;2;248;248;242m*[0m[38;2;249;38;114m>[0m[38;2;248;248;242m arbitrary[0m
