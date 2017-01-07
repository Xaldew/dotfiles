#!/usr/bin/env sh

# Install Rust compiler, runtime and packaging tools.
if ! command -v rustc >/dev/null 2>&1; then
    curl -sSf https://sh.rustup.rs | sh
fi

# Install various Rust utilities.
if command -v cargo; then
    cargo install racer
    cargo install rustfmt
fi

# Download the Rust source for code completion with Racer.
if [ ! -d $objects_dir/rust ]; then
    git clone https://github.com/rust-lang/rust.git $objects_dir/rust
else
    git -C $objects_dir/rust pull
fi
