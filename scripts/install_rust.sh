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


# Use rustup to setup the Rust toolchain(s).
if command -v rustup; then

    rustup install nightly

    rustup component add rls --toolchain nightly
    rustup component add rust-analysis --toolchain nightly
    rustup component add rust-src --toolchain nightly

    # Install bash completion file.
    rustup completions bash > $HOME/.bash_completion.d/rustup.bash-completion

    # Install Cargo bash completion.
    toolchain_dir=$(dirname $(dirname $(rustup which cargo)))
    cp $toolchain_dir/etc/cargo.bashcomp.sh $HOME/.bash_completion.d/
fi

# Download the Rust source for standard code completion with Racer.
if [ ! -d $objects_dir/rust ]; then
    git clone https://github.com/rust-lang/rust.git $objects_dir/rust
else
    git -C $objects_dir/rust pull
fi
