[38;2;249;38;114mimport[0m[38;2;248;248;242m data.matrix.notation[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m data.vector2[0m

[38;2;117;113;94m/-![0m

[38;2;117;113;94mHelpers that don't currently fit elsewhere...[0m

[38;2;117;113;94m-/[0m

[38;2;249;38;114mlemma[0m[38;2;248;248;242m [0m[38;2;166;226;46msplit_eq[0m[38;2;248;248;242m [0m[38;2;248;248;242m{m n : [0m[3;38;2;102;217;239mType[0m[38;2;248;248;242m*} (x : m × n) (p p' : m × n) :[0m
[38;2;248;248;242m  p = x ∨ p' = x ∨ (x ≠ p ∧ x ≠ p') := [0m[38;2;249;38;114mby[0m[38;2;248;248;242m tauto[0m

[38;2;117;113;94m--[0m[38;2;117;113;94m For `playfield`s, the piece type and/or piece index type.[0m
[38;2;249;38;114mvariables[0m[38;2;248;248;242m (X : [0m[3;38;2;102;217;239mType[0m[38;2;248;248;242m*)[0m
[38;2;249;38;114mvariables[0m[38;2;248;248;242m [has_repr X][0m

[38;2;249;38;114mnamespace[0m[38;2;248;248;242m chess.utils[0m

[38;2;249;38;114msection[0m[38;2;248;248;242m repr[0m

[38;2;117;113;94m/--[0m
[38;2;117;113;94mAn auxiliary wrapper for `option X` that allows for overriding the `has_repr` instance[0m
[38;2;117;113;94mfor `option`, and rather, output just the value in the `some` and a custom provided[0m
[38;2;117;113;94m`string` for `none`.[0m
[38;2;117;113;94m-/[0m
[38;2;249;38;114mstructure[0m[38;2;248;248;242m [0m[38;2;166;226;46moption_wrapper[0m[38;2;248;248;242m [0m[38;2;248;248;242m:=[0m
[38;2;248;248;242m(val : option X)[0m
[38;2;248;248;242m(none_s : string)[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;166;226;46mwrapped_option_repr[0m[38;2;248;248;242m [0m[38;2;248;248;242m: has_repr (option_wrapper X) :=[0m
[38;2;248;248;242m⟨[0m[38;2;249;38;114mλ[0m[38;2;248;248;242m ⟨val, s⟩, (option.map has_repr.repr val).get_or_else s⟩[0m

[38;2;249;38;114mvariables[0m[38;2;248;248;242m {X}[0m
[38;2;117;113;94m/--[0m
[38;2;117;113;94mConstruct an `option_wrapper` term from a provided `option X` and the `string`[0m
[38;2;117;113;94mthat will override the `has_repr.repr` for `none`.[0m
[38;2;117;113;94m-/[0m
[38;2;249;38;114mdef[0m[38;2;248;248;242m [0m[38;2;166;226;46moption_wrap[0m[38;2;248;248;242m [0m[38;2;248;248;242m(val : option X) (none_s : string) : option_wrapper X := ⟨val, none_s⟩[0m

[38;2;117;113;94m--[0m[38;2;117;113;94m The size of the "vectors" for a `fin n' → X`, for `has_repr` definitions[0m
[38;2;249;38;114mvariables[0m[38;2;248;248;242m {m' n' : ℕ}[0m

[38;2;117;113;94m/--[0m
[38;2;117;113;94mFor a "vector" `X^n'` represented by the type `Π n' : ℕ, fin n' → X`, where[0m
[38;2;117;113;94mthe `X` has a `has_repr` instance itself, we can provide a `has_repr` for the "vector".[0m
[38;2;117;113;94mThis definition is used for displaying rows of the playfield, when it is defined[0m
[38;2;117;113;94mvia a `matrix`, likely through notation.[0m
[38;2;117;113;94m-/[0m
[38;2;249;38;114mdef[0m[38;2;248;248;242m [0m[38;2;166;226;46mvec_repr[0m[38;2;248;248;242m [0m[38;2;248;248;242m: Π {n' : ℕ}, (fin n' → X) → string :=[0m
[38;2;249;38;114mλ[0m[38;2;248;248;242m _ v, string.intercalate [0m[38;2;230;219;116m"[0m[38;2;230;219;116m, [0m[38;2;230;219;116m"[0m[38;2;248;248;242m ((vector.of_fn v).to_list.map repr)[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;166;226;46mvec_repr_instance[0m[38;2;248;248;242m [0m[38;2;248;248;242m: has_repr (fin n' → X) := ⟨vec_repr⟩[0m

[38;2;117;113;94m/--[0m
[38;2;117;113;94mFor a `matrix` `X^(m' × n')` where the `X` has a `has_repr` instance itself,[0m
[38;2;117;113;94mwe can provide a `has_repr` for the matrix, using `vec_repr` for each of the rows of the matrix.[0m
[38;2;117;113;94mThis definition is used for displaying the playfield, when it is defined[0m
[38;2;117;113;94mvia a `matrix`, likely through notation.[0m
[38;2;117;113;94m-/[0m
[38;2;249;38;114mdef[0m[38;2;248;248;242m [0m[38;2;166;226;46mmatrix_repr[0m[38;2;248;248;242m [0m[38;2;248;248;242m: Π {m' n'}, matrix (fin m') (fin n') X → string :=[0m
[38;2;249;38;114mλ[0m[38;2;248;248;242m _ _ M, string.intercalate [0m[38;2;230;219;116m"[0m[38;2;230;219;116m;[0m[38;2;190;132;255m\n[0m[38;2;230;219;116m"[0m[38;2;248;248;242m ((vector.of_fn M).to_list.map repr)[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m [0m[38;2;166;226;46mmatrix_repr_instance[0m[38;2;248;248;242m [0m[38;2;248;248;242m:[0m
[38;2;248;248;242m  has_repr (matrix (fin n') (fin m') X) := ⟨matrix_repr⟩[0m

[38;2;249;38;114mend[0m[38;2;248;248;242m repr[0m

[38;2;249;38;114mend[0m[38;2;248;248;242m chess.utils[0m
