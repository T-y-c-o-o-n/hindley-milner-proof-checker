# hindley-milner-proof-checker

Checker for correctness of proof
in [Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system).

### Rules

| Rule | Dependencies                   | Result                      | Requirements                              |
|------|--------------------------------|-----------------------------|-------------------------------------------|
| 1    |                                | `Γ ⊢ x : σ`                 | `x : σ ∈ Γ`                               |
| 2    | `Γ ⊢ f : τ → τ'` `Γ ⊢ e : τ`   | `Γ ⊢ f e : τ'`              | `τ` `τ'` are monotypes                    |
| 3    | `Γ, x : τ ⊢ e : τ'`            | `Γ, x : τ ⊢ λx. e : τ → τ'` | `τ` `τ'` are monotypes, `x` is a variable |
| 4    | `Γ ⊢ e : σ` `Γ, x : σ ⊢ f : τ` | `Γ ⊢ let x = e in f : τ`    | `τ` is a monotype, `x` is a variable      | 
| 5    | `Γ ⊢ e : σ'`                   | `Γ ⊢ e : σ`                 | `σ` is subtype of `σ'`                    |
| 6    | `Γ ⊢ e : σ`                    | `Γ ⊢ e : ∀α. σ`             | `α ∉ ` free type variables of `Γ`         |