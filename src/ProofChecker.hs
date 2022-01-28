module ProofChecker where

import Base
import qualified Data.Map as M
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
checkSubtype sigma' (ForAll b t) = b `notMember` freeVars sigma' && checkSubtype sigma' t
checkSubtype sigma' (Mono t) = fst $ checkSubtype' M.empty sigma' t
  where
    checkSubtype' :: M.Map Var (Maybe MonoType) -> Type -> MonoType -> (Bool, M.Map Var (Maybe MonoType))
    checkSubtype' substitutions (ForAll a t0) t1 = checkSubtype' (M.insert a Nothing substitutions) t0 t1
    checkSubtype' substitutions (Mono t1@(V a)) t2 =
      case a `M.lookup` substitutions of
        Nothing -> (t1 == t2, substitutions)
        Just Nothing -> (True, M.insert a (Just t2) substitutions)
        Just (Just t') -> (t' == t2, substitutions)
    checkSubtype' substitutions (Mono (t0 :=> t1)) (t0' :=> t1') =
      let left@(leftRec, substitutions') = checkSubtype' substitutions (Mono t0) t0'
       in if leftRec
            then checkSubtype' substitutions' (Mono t1) t1'
            else left
    checkSubtype' s _ _ = (False, s)

checkProof :: ProofTree -> Bool
checkProof ([] `Proof` hs :|- ax@(Var _ :. _) :# 1) = ax `elem` hs
-- rule 1

checkProof
  ( ch@[ _ `Proof` hs :|- e0 :. Mono (t0 :=> t1) :# _,
         _ `Proof` hs' :|- e1 :. Mono t0' :# _
         ]
      `Proof` (hs'' :|- e0' `Appl` e1' :. Mono t1')
      :# 2
    ) =
    all checkProof ch
      && hs == hs'
      && hs' == hs''
      && e0 == e0'
      && e1 == e1'
      && t0 == t0'
      && t1 == t1'
-- rule 2

checkProof (ch@[_ `Proof` ((Var x' :. Mono t0') : hs) :|- e' :. Mono t1' :# _] `Proof` (hs' :|- L x e :. Mono (t0 :=> t1)) :# 3) =
  all checkProof ch
    && hs == hs'
    && x == x'
    && e == e'
    && t1 == t1'
    && t0 == t0'
-- rule 3

checkProof
  ( ch@[ _ `Proof` hs :|- e0 :. sigma :# _,
         _ `Proof` ((Var x' :. sigma') : hs') :|- e1 :. Mono t :# _
         ]
      `Proof` (hs'' :|- Let x e0' e1' :. Mono t' :# 4)
    ) =
    all checkProof ch
      && hs == hs'
      && hs' == hs''
      && x == x'
      && e0 == e0'
      && e1 == e1'
      && t == t'
      && sigma == sigma'
-- rule 4

checkProof (ch@[_ `Proof` hs :|- e :. sigma' :# _] `Proof` hs' :|- e' :. sigma :# 5) =
  all checkProof ch
    && hs == hs'
    && e == e'
    && checkSubtype sigma' sigma
-- rule 5

checkProof (ch@[_ `Proof` hs :|- e :. sigma :# _] `Proof` hs' :|- e' :. ForAll a sigma' :# 6) =
  all checkProof ch
    && hs == hs'
    && e == e'
    && sigma == sigma'
    && a `notElem` freeVarsFromContext hs
-- rule 6

checkProof _ = False

-- a : forall x. x, b : y |- let huy = a in b : y [rule #4]

-- *   a : forall x. x, b : y |- a : forall x. x [rule #1]

-- *   a : forall x. x, b : y, huy : forall x. x |- b : y [rule #1]
