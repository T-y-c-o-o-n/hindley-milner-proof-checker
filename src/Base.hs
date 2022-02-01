{-# LANGUAGE GADTs #-}

module Base where

infix 2 `Proof`

infix 3 :#

infix 4 :|-

infix 5 :.

infixr 6 :=>

infix 7 `Appl`

type Var = String

data Expr where
  Var :: Var -> Expr
  L :: Var -> Expr -> Expr
  Appl :: Expr -> Expr -> Expr
  Let :: Var -> Expr -> Expr -> Expr
  deriving (Eq, Ord, Show)

data Type where
  ForAll :: Var -> Type -> Type
  Mono :: MonoType -> Type
  deriving (Eq, Ord, Show)

data MonoType where
  (:=>) :: MonoType -> MonoType -> MonoType
  V :: Var -> MonoType
  deriving (Eq, Ord, Show)

data TypedExpr = Expr :. Type
  deriving (Eq, Ord, Show)

type Context = [TypedExpr]

data ProofNode = ProofNode' :# Int
  deriving (Eq, Ord, Show)

data ProofNode' = Context :|- TypedExpr
  deriving (Eq, Ord, Show)

data ProofTree = [ProofTree] `Proof` ProofNode
  deriving (Eq, Ord, Show)
