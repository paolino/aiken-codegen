# aiken-codegen

Haskell DSL for generating Aiken source code.

## Goals

- **Problem**: Generating Aiken source code from Haskell requires ad-hoc
  string concatenation — fragile, hard to maintain, not composable.
- **Solution**: A minimal Haskell AST for the Aiken language subset needed
  for code generation (tests, records, function calls, let bindings,
  literals). Construct expressions as data, render to source, let
  `aiken fmt` normalize.
- **Non-goal**: Full Aiken language coverage. This covers the subset useful
  for cross-language test vector generation and similar codegen tasks.
- **Usage**: Build Aiken `Module` values from `Expr` and `Def`
  constructors, call `renderModule` to get source text.

## Build

```bash
nix develop -c just build
nix develop -c just unit
nix develop -c just CI
```

## License

Apache-2.0
