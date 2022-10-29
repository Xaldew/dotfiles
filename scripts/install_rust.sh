#!/usr/bin/env sh

# Install Rust compiler, runtime and packaging tools.
if ! command -v rustc >/dev/null 2>&1; then
    curl --proto '=https' --tlsv1.2 https://sh.rustup.rs -sSf | sh
fi


# Install various Rust utilities.
if command -v cargo; then
    cargo +nightly install --force cargo-profiler
fi


# Use rustup to setup the Rust toolchain(s).
if command -v rustup; then

    rustup update
    rustup install nightly

    rustup component add rustfmt --toolchain nightly
    rustup component add rls --toolchain nightly
    rustup component add clippy --toolchain nightly
    rustup component add rust-analysis --toolchain nightly
    rustup component add rust-src --toolchain nightly
    rustup component add rustc-dev --toolchain nightly
    rustup component add rust-analyzer

    # Stable Rust
    rustup component add rustfmt
    rustup component add rust-analyzer
    rustup component add rls
    rustup component add clippy
    rustup component add rust-analysis
    rustup component add rust-analyzer
    rustup component add rust-src

    # Install bash completion file.
    rustup completions bash > $HOME/.bash_completion.d/rustup.bash-completion

    # Add link for rust-analyzer to PATH.
    ln -f -s $(rustup which --toolchain stable rust-analyzer) $HOME/.cargo/bin/rust-analyzer

    # Install Cargo bash completion.
    toolchain_dir=$(dirname $(dirname $(rustup which cargo)))
    cp $toolchain_dir/etc/bash_completion.d/cargo $HOME/.bash_completion.d/
fi
