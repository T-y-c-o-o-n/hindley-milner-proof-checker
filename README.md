# hindley-milner-proof-checker

---

Checker for proof correctness
in [Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system).

### Type Grammar

#### Regular types

`α`

#### Monotypes

`τ ::= α | τ → τ`

#### Polytypes

`σ ::= ∀α. σ | τ`

### Rules

| Rule | Dependencies                    | Result                   | Requirements                              |
|------|---------------------------------|--------------------------|-------------------------------------------|
| 1    |                                 | `Γ ⊢ x : σ`              | `x : σ ∈ Γ`, `x` is a variable            |
| 2    | `Γ ⊢ f : τ → τ'` `Γ ⊢ e : τ`    | `Γ ⊢ f e : τ'`           | `τ` `τ'` are monotypes                    |
| 3    | `Γ, x : τ ⊢ e : τ'`             | `Γ ⊢ λx. e : τ → τ'`     | `τ` `τ'` are monotypes, `x` is a variable |
| 4    | `Γ ⊢ e : σ` `Γ, x : σ ⊢ f : τ`  | `Γ ⊢ let x = e in f : τ` | `τ` is a monotype, `x` is a variable      | 
| 5    | `Γ ⊢ e : σ'`                    | `Γ ⊢ e : σ`              | `σ` is subtype of `σ'`                    |
| 6    | `Γ ⊢ e : σ`                     | `Γ ⊢ e : ∀α. σ`          | `α ∉` free type variables of `Γ`          |

## Input

Several lines, each of them is step of proof with rule annotation at the end. Line has 0 or more indents. Indent is `*` + 3 spaces.
The children of the line are lines with 1 more indent after her and before line with indent less or equal her.
The children are expected to be Dependencies from the Rule table in same order.

### Example
```
w : t1, y : t' |- let c = \x. \z. x in c w y : t1 [rule #4]
*   w : t1, y : t' |- \x. \z. x : forall a. t1 -> a -> t1 [rule #6]
*   *   w : t1, y : t' |- \x. \z. x : t1 -> a -> t1 [rule #3]
*   *   *   w : t1, y : t', x : t1 |- \z. x : a -> t1 [rule #3]
*   *   *   *   w : t1, y : t', x : t1, z : a |- x : t1 [rule #1]
*   w : t1, y : t', c : forall a. t1 -> a -> t1 |- c w y : t1 [rule #2]
*   *   w : t1, y : t', c : forall a. t1 -> a -> t1 |- c w : t' -> t1 [rule #2]
*   *   *   w : t1, y : t', c : forall a. t1 -> a -> t1 |- c : t1 -> t' -> t1 [rule #5]
*   *   *   *   w : t1, y : t', c : forall a. t1 -> a -> t1 |- c : forall b. t1 -> b -> t1 [rule #5]
*   *   *   *   *   w : t1, y : t', c : forall a. t1 -> a -> t1 |- c : forall a. t1 -> a -> t1 [rule #1]
*   *   *   w : t1, y : t', c : forall a. t1 -> a -> t1 |- w : t1 [rule #1]
*   *   w : t1, y : t', c : forall a. t1 -> a -> t1 |- y : t' [rule #1]
```

## Output

`Correct` / `Incorrect` or parse error