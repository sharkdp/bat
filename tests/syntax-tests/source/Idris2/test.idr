-- some code in Idris
module XX.X'''

import Data.Nat

data X = A | B

namespace X
  ||| Documentation
  record Y where
    [noHints]
    constructor MkY'
    field1 : Nat
    {auto x : Nat}

namespace X' {
  parameters (x : A (Maybe b))
    x : Nat
}

u : ()
u = ()

k, w, u : Char
k = '\NUL'
w = 'w'

x = [1, 0, 3, "sdf\{d}", 0xFF, 0o77, 0b10_1, 100_100]

f : Int -> Int
f = if x > 0 then x else 0 () SS `elem` S $ do
  x <- a [1, 2, 3]
  let ukuk = akak
  rewrite $ Wow Wow Wow Wow.Wow b W (W)
  pure $ f A B c D (EE) E

(&&&) : Nat -> Nat -> Nat
z &&& y = d + ?foo
(&&&) x y = ?asfda

public export covering
(.fun) : X a Y b => Nat -> Nat
Z .fun = haha.fun haha .N
(.fun) Z = ahah $ \case
  x@(x, y) => Prelude.Types.ahahah

(.N) : Nat -> Nat
Z .N = Z
(.N) (S n) = (.N) n

xx : Name
xx = `{Full.Name}

infixr 0 ^^^, &&&

xxx : ?
xxx = case x of
  Z => lalalaCamelCase
  z => alalalCamelCase

ff : Nat -> TTImp
ff 0 = let x = 0 in val
ff _ = `(let x = 0 in ~val ^~^ ~(abc))
ff _ = f `(let x = 0 in ~val ^~^ ~(abc)) x

%language ElabReflection
%runElab X.sf ads

%macro %inline
fff : List Decl
fff = `[
  f : Nat -> Nat

  f Z = haha %runElab %search @{%World}
]

private infixr 4 ^--^

(^--^) : Nat -> Nat -> Nat
(^--^) Z Z = Z
x ^--^ y = x + y

x : (y : Vect n (Maybe (Maybe (&&&) Nat))) ->
    {x : Nat} -> {auto _ : Monoid a} ->
    {default 4 xx : Nat} ->
    {default (f x Y) xx' : Nat} ->
    String
x Z S = ?foo
x y _ = "a b \{show $ let x = 0 in y} y >>= z"

multiline : String
multiline = """
  A multiline string\NUL
  """

f' : Nat -> Nat
f' = x' 4

x : Char
x = '\BEL'
x = '\\'
x = '\''
x = '\o755'
x = 'a'

xx : Int
xx = 0o7_5_5
