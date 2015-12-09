#!/usr/bin/env sh

# Install Rust compiler, runtime and packaging tools.
if ! command -v rustc >/dev/null 2>&1; then
    tmp_dir=`mktemp --directory`
    curl -sSf https://static.rust-lang.org/rustup.sh > $tmp_dir/rustup.sh
    sh $tmp_dir/rustup.sh
    rm -r $tmp_dir
fi
