module basicInduction

data Nat : Type where
  Z : Nat
  S : Nat -> Nat

natural_induction : (P : Nat -> Type) ->
                    (P Z) ->
                    ((k : Nat) -> P k -> P (S k)) ->
                    (x : Nat) ->
                    P x

natural_induction P p_Z p_S Z = p_Z
natural_induction P p_Z p_S (S k) = p_S k (natural_induction P p_Z p_S k)

plus : Nat -> Nat -> nat
plus n m = 
  natural_induction (x => Nat)
                    m
                    (k k_recursive => S k_recursive)
                    n

-- commutative proof for plus
plus_commutes : (n : Nat) -> (m : Nat) -> n + m = m + n

-- base case
plus_commutes_Z : m = plus m 0
plus_commutes_Z {m = Z} = Refl
plus_commutes_Z {m = (S k)} = let rec = plus_commutes_Z {m = k} in 
                                  rewrite rec in Refl

total 
plus_commutes_S : (k : Nat) -> (m : Nat) -> S (plus m k) = plus m (S k)
plus_commutes_S k Z = Refl
plus_commutes_S k (S j) = rewrite plus_commutes_S k j in Refl
