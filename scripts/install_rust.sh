#!/usr/bin/env sh

# Install Rust compiler, runtime and packaging tools.
if ! command -v rustc >/dev/null 2>&1; then
    tmp_dir=`mktemp --directory`
    curl -sSf https://static.rust-lang.org/rustup.sh > $tmp_dir/rustup.sh
    sh $tmp_dir/rustup.sh
    rm -r $tmp_dir
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
