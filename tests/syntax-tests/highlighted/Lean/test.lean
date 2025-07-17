[38;2;249;38;114mimport[0m[38;2;248;248;242m MIL.Common[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m Mathlib.Topology.Instances.Real.Defs[0m

[38;2;249;38;114mopen[0m[38;2;248;248;242m Set Filter Topology[0m

[38;2;249;38;114mvariable[0m[38;2;248;248;242m {α : [0m[3;38;2;102;217;239mType[0m[38;2;248;248;242m*}[0m
[38;2;249;38;114mvariable[0m[38;2;248;248;242m (s t : Set ℕ)[0m
[38;2;249;38;114mvariable[0m[38;2;248;248;242m (ssubt : s ⊆ t)[0m
[38;2;249;38;114mvariable[0m[38;2;248;248;242m {α : [0m[3;38;2;102;217;239mType[0m[38;2;248;248;242m*} (s : Set (Set α))[0m
[38;2;117;113;94m-- Apostrophes are allowed in variable names[0m
[38;2;249;38;114mvariable[0m[38;2;248;248;242m (f'_x x' : ℕ)[0m
[38;2;249;38;114mvariable[0m[38;2;248;248;242m (bangwI' jablu'DI' QaQqu' nay' Ghay'cha' he' : ℕ)[0m

[38;2;117;113;94m-- In the next example we could use `tauto` in each proof instead of knowing the lemmas[0m
[38;2;249;38;114mexample[0m[38;2;248;248;242m {α : [0m[3;38;2;102;217;239mType[0m[38;2;248;248;242m*} (s : Set α) : Filter α :=[0m
[38;2;248;248;242m  { sets := { t | s ⊆ t }[0m
[38;2;248;248;242m    univ_sets := subset_univ s[0m
[38;2;248;248;242m    sets_of_superset := [0m[38;2;249;38;114mfun[0m[38;2;248;248;242m hU hUV ↦ Subset.trans hU hUV[0m
[38;2;248;248;242m    inter_sets := [0m[38;2;249;38;114mfun[0m[38;2;248;248;242m hU hV ↦ subset_inter hU hV }[0m


[38;2;249;38;114mnamespace[0m[38;2;248;248;242m chess.utils[0m

[38;2;249;38;114msection[0m[38;2;248;248;242m repr[0m

[38;2;249;38;114m@[class][0m[38;2;248;248;242m [0m[38;2;249;38;114mstructure[0m[38;2;248;248;242m [0m[38;2;166;226;46mOne₂[0m[38;2;248;248;242m [0m[38;2;248;248;242m(α : [0m[3;38;2;102;217;239mType[0m[38;2;248;248;242m) [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  [0m[38;2;117;113;94m/-- The element one -/[0m
[38;2;248;248;242m  one : α[0m

[38;2;249;38;114mstructure[0m[38;2;248;248;242m [0m[38;2;166;226;46mStandardTwoSimplex[0m[38;2;248;248;242m [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  x : ℝ[0m
[38;2;248;248;242m  y : ℝ[0m
[38;2;248;248;242m  z : ℝ[0m
[38;2;248;248;242m  x_nonneg : [0m[38;2;190;132;255m0[0m[38;2;248;248;242m ≤ x[0m
[38;2;248;248;242m  y_nonneg : [0m[38;2;190;132;255m0[0m[38;2;248;248;242m ≤ y[0m
[38;2;248;248;242m  z_nonneg : [0m[38;2;190;132;255m0[0m[38;2;248;248;242m ≤ z[0m
[38;2;248;248;242m  sum_eq : x + y + z = [0m[38;2;190;132;255m1[0m

[38;2;249;38;114m#check[0m[38;2;248;248;242m Pi.ringHom[0m
[38;2;249;38;114m#check[0m[38;2;248;248;242m ker_Pi_Quotient_mk[0m
[38;2;249;38;114m#eval[0m[38;2;248;248;242m [0m[38;2;190;132;255m1[0m[38;2;248;248;242m + [0m[38;2;190;132;255m1[0m

[38;2;117;113;94m/-- The homomorphism from ``R ⧸ ⨅ i, I i`` to ``Π i, R ⧸ I i`` featured in the Chinese[0m
[38;2;117;113;94m  Remainder Theorem. -/[0m
[38;2;249;38;114mdef[0m[38;2;248;248;242m [0m[38;2;166;226;46mchineseMap[0m[38;2;248;248;242m [0m[38;2;248;248;242m(I : ι → Ideal R) : (R ⧸ ⨅ i, I i) →+* Π i, R ⧸ I i :=[0m
[38;2;248;248;242m  Ideal.Quotient.lift (⨅ i, I i) (Pi.ringHom [0m[38;2;249;38;114mfun[0m[38;2;248;248;242m i : ι ↦ Ideal.Quotient.mk (I i))[0m
[38;2;248;248;242m    ([0m[38;2;249;38;114mby[0m[38;2;248;248;242m simp [← RingHom.mem_ker, ker_Pi_Quotient_mk])[0m

[38;2;249;38;114mlemma[0m[38;2;248;248;242m [0m[38;2;166;226;46mchineseMap_mk[0m[38;2;248;248;242m [0m[38;2;248;248;242m(I : ι → Ideal R) (x : R) :[0m
[38;2;248;248;242m    chineseMap I (Quotient.mk _ x) = [0m[38;2;249;38;114mfun[0m[38;2;248;248;242m i : ι ↦ Ideal.Quotient.mk (I i) x :=[0m
[38;2;248;248;242m  rfl[0m

[38;2;249;38;114mtheorem[0m[38;2;248;248;242m [0m[38;2;166;226;46misCoprime_Inf[0m[38;2;248;248;242m [0m[38;2;248;248;242m{I : Ideal R} {J : ι → Ideal R} {s : Finset ι}[0m
[38;2;248;248;242m    (hf : ∀ j ∈ s, IsCoprime I (J j)) : IsCoprime I (⨅ j ∈ s, J j) := [0m[38;2;249;38;114mby[0m
[38;2;248;248;242m  classical[0m
[38;2;248;248;242m  simp_rw [isCoprime_iff_add] at *[0m
[38;2;248;248;242m  induction s using Finset.induction [0m[38;2;249;38;114mwith[0m
[38;2;248;248;242m  | empty =>[0m
[38;2;248;248;242m      simp[0m
[38;2;248;248;242m  | @insert i s _ hs =>[0m
[38;2;248;248;242m      rw [Finset.iInf_insert, inf_comm, one_eq_top, eq_top_iff, ← one_eq_top][0m
[38;2;248;248;242m      set K := ⨅ j ∈ s, J j[0m
[38;2;248;248;242m      [0m[38;2;249;38;114mcalc[0m
[38;2;248;248;242m        [0m[38;2;190;132;255m1[0m[38;2;248;248;242m = I + K                  := (hs [0m[38;2;249;38;114mfun[0m[38;2;248;248;242m j hj ↦ hf j (Finset.mem_insert_of_mem hj)).symm[0m
[38;2;248;248;242m        _ = I + K * (I + J i)      := [0m[38;2;249;38;114mby[0m[38;2;248;248;242m rw [hf i (Finset.mem_insert_self i s), mul_one][0m
[38;2;248;248;242m        _ = ([0m[38;2;190;132;255m1[0m[38;2;248;248;242m + K) * I + K * J i  := [0m[38;2;249;38;114mby[0m[38;2;248;248;242m ring[0m
[38;2;248;248;242m        _ ≤ I + K ⊓ J i            := [0m[38;2;249;38;114mby[0m[38;2;248;248;242m gcongr ; apply mul_le_left ; apply mul_le_inf[0m


[38;2;249;38;114mclass[0m[38;2;248;248;242m [0m[38;2;166;226;46mRing₃[0m[38;2;248;248;242m [0m[38;2;248;248;242m(R : [0m[3;38;2;102;217;239mType[0m[38;2;248;248;242m) [0m[38;2;249;38;114mextends[0m[38;2;248;248;242m AddGroup₃ R, Monoid₃ R, MulZeroClass R [0m[38;2;249;38;114mwhere[0m
[38;2;248;248;242m  [0m[38;2;117;113;94m/-- Multiplication is left distributive over addition -/[0m
[38;2;248;248;242m  left_distrib : ∀ a b c : R, a * (b + c) = a * b + a * c[0m
[38;2;248;248;242m  [0m[38;2;117;113;94m/-- Multiplication is right distributive over addition -/[0m
[38;2;248;248;242m  right_distrib : ∀ a b c : R, (a + b) * c = a * c + b * c[0m

[38;2;249;38;114minstance[0m[38;2;248;248;242m {R : Type} [0m[38;2;248;248;242m[Ring₃ R] : AddCommGroup₃ R :=[0m
[38;2;248;248;242m{ Ring₃.toAddGroup₃ [0m[38;2;249;38;114mwith[0m
[38;2;248;248;242m  add_comm := [0m[38;2;249;38;114mby[0m
[38;2;248;248;242m    [0m[38;2;248;248;240msorry[0m[38;2;248;248;242m }[0m

[38;2;249;38;114mend[0m[38;2;248;248;242m repr[0m

[38;2;249;38;114mend[0m[38;2;248;248;242m chess.utils[0m
