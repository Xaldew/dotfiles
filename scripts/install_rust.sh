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


# Use rustup to install setup Rust.
if command -v rustup; then

    rustup install nightly

    # Install bash completion file.
    tmp=$(mktemp)
    rustup completions bash > $tmp
    sudo mv $tmp /etc/bash_completion.d/rustup.bash-completion
fi


# Download the Rust source for code completion with Racer.
if [ ! -d $objects_dir/rust ]; then
    git clone https://github.com/rust-lang/rust.git $objects_dir/rust
else
    git -C $objects_dir/rust pull
fi
