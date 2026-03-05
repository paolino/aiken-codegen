# shellcheck shell=bash

set unstable := true

# List available recipes
default:
    @just --list

# Format all source files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    for i in {1..3}; do
        fourmolu -i lib test
    done
    cabal-fmt -i *.cabal
    nixfmt *.nix nix/*.nix

# Check formatting without modifying
format-check:
    #!/usr/bin/env bash
    set -euo pipefail
    fourmolu -m check lib test
    cabal-fmt -c *.cabal
    nixfmt -c *.nix nix/*.nix

# Run hlint
hlint:
    #!/usr/bin/env bash
    hlint lib test

# Build all components
build:
    #!/usr/bin/env bash
    cabal build all --enable-tests -O0

# Run golden tests with optional match pattern
unit match="":
    #!/usr/bin/env bash
    if [[ '{{ match }}' == "" ]]; then
        cabal test golden-tests -O0 \
            --test-show-details=direct
    else
        cabal test golden-tests -O0 \
            --test-show-details=direct \
            --test-option=--match \
            --test-option="{{ match }}"
    fi

# Update golden files
update-golden:
    #!/usr/bin/env bash
    UPDATE_GOLDEN=1 cabal test golden-tests -O0 \
        --test-show-details=direct

# Full CI pipeline
CI:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just unit
    fourmolu -m check lib test
    hlint lib test
