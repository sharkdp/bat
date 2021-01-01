import data.matrix.notation
import data.vector2

/-!

Helpers that don't currently fit elsewhere...

-/

lemma split_eq {m n : Type*} (x : m × n) (p p' : m × n) :
  p = x ∨ p' = x ∨ (x ≠ p ∧ x ≠ p') := by tauto

-- For `playfield`s, the piece type and/or piece index type.
variables (X : Type*)
variables [has_repr X]

namespace chess.utils

section repr

/--
An auxiliary wrapper for `option X` that allows for overriding the `has_repr` instance
for `option`, and rather, output just the value in the `some` and a custom provided
`string` for `none`.
-/
structure option_wrapper :=
(val : option X)
(none_s : string)

instance wrapped_option_repr : has_repr (option_wrapper X) :=
⟨λ ⟨val, s⟩, (option.map has_repr.repr val).get_or_else s⟩

variables {X}
/--
Construct an `option_wrapper` term from a provided `option X` and the `string`
that will override the `has_repr.repr` for `none`.
-/
def option_wrap (val : option X) (none_s : string) : option_wrapper X := ⟨val, none_s⟩

-- The size of the "vectors" for a `fin n' → X`, for `has_repr` definitions
variables {m' n' : ℕ}

/--
For a "vector" `X^n'` represented by the type `Π n' : ℕ, fin n' → X`, where
the `X` has a `has_repr` instance itself, we can provide a `has_repr` for the "vector".
This definition is used for displaying rows of the playfield, when it is defined
via a `matrix`, likely through notation.
-/
def vec_repr : Π {n' : ℕ}, (fin n' → X) → string :=
λ _ v, string.intercalate ", " ((vector.of_fn v).to_list.map repr)

instance vec_repr_instance : has_repr (fin n' → X) := ⟨vec_repr⟩

/--
For a `matrix` `X^(m' × n')` where the `X` has a `has_repr` instance itself,
we can provide a `has_repr` for the matrix, using `vec_repr` for each of the rows of the matrix.
This definition is used for displaying the playfield, when it is defined
via a `matrix`, likely through notation.
-/
def matrix_repr : Π {m' n'}, matrix (fin m') (fin n') X → string :=
λ _ _ M, string.intercalate ";\n" ((vector.of_fn M).to_list.map repr)

instance matrix_repr_instance :
  has_repr (matrix (fin n') (fin m') X) := ⟨matrix_repr⟩

end repr

end chess.utils
