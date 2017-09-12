# Environment settings common to all Posix shells.

# Set XDG directory variables.
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME-$HOME/.config}
export XDG_DATA_HOME=${XDG_DATA_HOME-"${local_prefix_dir}/share"}

# Setup Emacs as the default editor.
export EDITOR=nx_emacs_client
export VISUAL=graphical_emacs_client

# Add extra man-page directories.
export MANPATH=${XDG_DATA_HOME}/man:$MANPATH
export INFOPATH=${XDG_DATA_HOME}/info:$INFOPATH


# Add user paths to PATH so it includes private binaries.

## Add Android SDK/NDK paths.
if [ -d "$objects_dir/android_sdk/tools" ]; then
    PATH="$objects_dir/android_sdk/tools":$PATH
fi
if [ -d "$objects_dir/android_sdk/platform-tools" ]; then
    PATH="$objects_dir/android_sdk/platform-tools":$PATH
fi
if [ -d "$objects_dir/android_ndk/" ]; then
    PATH="$objects_dir/android_ndk":$PATH
fi

## Add paths to the Go language toolchain.
if [ -d "$objects_dir/golang" ]; then
    export GOROOT="$objects_dir/golang"
    PATH=$PATH:$GOROOT/bin
fi

## Add Emacs Cask and version managers.
if [ -d "$objects_dir/cask" ]; then
    PATH="$objects_dir/cask/bin":$PATH
fi
if [ -d "$objects_dir/evm" ]; then
    PATH="$objects_dir/evm/bin":$PATH
fi

## Add private scripts and compiled binaries.
if [ -d "$dotfiles_dir/scripts" ]; then
    PATH="$dotfiles_dir/scripts":$PATH
fi
if [ -d "$local_prefix_dir/bin" ]; then
    PATH="$local_prefix_dir/bin":$PATH
fi

## Add Rust binaries installed by Cargo.
if [ -d "$HOME/.cargo/bin" ]; then
    export RUST_SRC_PATH="$objects_dir/rust/src"
    PATH="$HOME/.cargo/bin":$PATH
fi