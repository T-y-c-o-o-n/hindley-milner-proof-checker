module ProofChecker where

import Base
import Data.Set

freeVars :: Type -> Set Var
freeVars = freeVars' empty
  where
    freeVars' :: Set Var -> Type -> Set Var
    freeVars' binded (ForAll x t) = freeVars' (x `insert` binded) t
    freeVars' binded (Mono (V a)) = if a `member` binded then empty else singleton a
    freeVars' binded (Mono (t0 :=> t1)) = freeVars' binded (Mono t0) `union` freeVars' binded (Mono t1)

freeVarsFromContext :: Context -> Set Var
freeVarsFromContext ((Var _ :. t) : xs) = freeVars t `union` freeVarsFromContext xs
freeVarsFromContext _ = empty

checkSubtype :: Type -> Type -> Bool
checkSubtype sigma' sigma = False

checkProof :: ProofTree -> Bool
checkProof ([] `Proof` _ :|- Var _ :. _ :# 1) = True
-- rule 1

checkProof
  ( [ _ `Proof` hs :|- e0 :. Mono (t0 :=> t1) :# _,
      _ `Proof` hs' :|- e1 :. Mono t0' :# _
      ]
      `Proof` (hs'' :|- e0' `Appl` e1' :. Mono t1')
      :# 2
    ) =
    hs == hs'
      && hs' == hs''
      && e0 == e0'
      && e1 == e1'
      && t0 == t0'
      && t1 == t1'
-- rule 2

checkProof ([_ `Proof` hs :|- e' :. Mono t1' :# _] `Proof` (hs' :|- L x e :. Mono (t0 :=> t1)) :# 3) =
  hs == hs'
    && e == e'
    && t1 == t1'
    && (Var x :. Mono t0) `elem` hs
-- rule 3

checkProof
  ( [ _ `Proof` hs :|- e0 :. sigma :# _,
      _ `Proof` hs' :|- e1 :. Mono t :# _
      ]
      `Proof` hs''
      :|- Let x e0' e1'
      :. Mono t'
      :# 4
    ) =
    hs == hs' && hs' == hs''
      && e0 == e0'
      && e1 == e1'
      && t == t'
      && (Var x :. sigma) `elem` hs'
-- rule 4

checkProof ([_ `Proof` hs :|- e :. sigma' :# _] `Proof` hs' :|- e' :. sigma :# 5) =
  hs == hs'
    && e == e'
    && checkSubtype sigma' sigma
-- rule 5

checkProof ([_ `Proof` hs :|- e :. sigma :# _] `Proof` hs' :|- e' :. ForAll x sigma' :# 6) =
  hs == hs'
    && e == e'
    && sigma == sigma'
    && x `notElem` freeVarsFromContext hs
-- rule 6

checkProof _ = False
