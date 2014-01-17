#!/bin/bash
# Install the utility scripts for a new computer or VM.

packages=()


# Declare some useful functions.
############################################################
function is_installed ()
{
    dpkg --status "$1" > /dev/null 2>&1
    return $?
}

function add_package ()
{
    for pkg in "$@"
    do
	if ! is_installed "$pkg"; then
	    packages+=("$pkg")
	fi
    done
}

function install_packages ()
{
    sudo apt-get --quiet install ${packages[*]}
}
#############################################################

# Install all software before installing configs.
# Install version management
add_package git subversion

# Install editors and terminal multiplexers.
add_package tmux vim ssh xclip

# Install C/C++ Compilers, Python Interpreter, build tools.
add_package gcc g++ clang valgrind python cmake

# Install GL development packages.
add_package freeglut3 freeglut3-dev libgl1-mesa-dev libglu1-mesa libglu1-mesa-dev
add_package libgl1-mesa-glx libx11-dev mesa-common-dev mesa-utils

# Install the Full latex-live distribution.
add_package texlive-full doxygen doxygen-latex auctex graphviz


# Install the accumulated packages.
install_packages

## Install all configuration files and plugins.
UTILS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export PATH="$UTILS_DIR/scripts":$PATH
ln -fs $UTILS_DIR/configs/inputrc $HOME/.inputrc
ln -fs $UTILS_DIR/configs/bash_aliases $HOME/.bash_aliases

# Install git and setup user gitconfig and gitignore.
ln -fs $UTILS_DIR/configs/gitconfig $HOME/.gitconfig
ln -fs $UTILS_DIR/configs/gitignore $HOME/.gitignore

# Install tmux configuration.
ln -fs $UTILS_DIR/configs/tmux.conf $HOME/.tmux.conf

# Install emacs configuration.
mkdir -p $HOME/.emacs.d
for elisp_file in $UTILS_DIR/configs/emacs.d/*.el
do
    ln -fs $elisp_file $HOME/.emacs.d/$(basename $elisp_file)
done
ln -fs $UTILS_DIR/configs/emacs.d/emacs $HOME/.emacs

# Install ethan-wspace for emacs from github.
if [ -d "$HOME/.emacs.d/ethan-wspace" ]; then
    (cd $HOME/.emacs.d/ethan-wspace && git pull --quiet)
else
    git clone https://github.com/glasserc/ethan-wspace.git \
	$HOME/.emacs.d/ethan-wspace
fi

# Install autopair add on.
if [ -d "$HOME/.emacs.d/autopair" ]; then
    (cd $HOME/.emacs.d/autopair && git pull --quiet)
else
    git clone https://github.com/capitaomorte/autopair.git \
	$HOME/.emacs.d/autopair
    git clone https://github.com/emacsmirror/auto-pair-plus.git /tmp/autopair+
    mv /tmp/autopair+/auto-pair+.el $HOME/.emacs.d/autopair
fi

# Install emacs Dot-mode for graphviz.
if [ -d "$HOME/.emacs.d/graphviz-dot-mode" ]; then
    (cd $HOME/.emacs.d/graphviz-dot-mode && git pull --quiet)
else
    git clone https://github.com/ppareit/graphviz-dot-mode.git \
	$HOME/.emacs.d/graphviz-dot-mode
fi

# Install emacs CMake mode and utilities.
if [ -d "$HOME/.emacs.d/emacs-cmake-project" ]; then
    (cd $HOME/.emacs.d/emacs-cmake-project && git pull --quiet)
    wget --quiet http://www.cmake.org/CMakeDocs/cmake-mode.el \
	--output-document=/tmp/cmake-mode.el
    new_cmake_mode=$(diff --brief \
	$HOME/.emacs.d/emacs-cmake-project/cmake-mode.el /tmp/cmake-mode.el)
    if [ -z "$new_cmake_mode" -a -e /tmp/cmake-mode.el ]; then
	mv --force /tmp/cmake-mode.el $HOME/.emacs.d/emacs-cmake-project/
    fi
else
    git clone https://github.com/alamaison/emacs-cmake-project.git \
	$HOME/.emacs.d/emacs-cmake-project
    wget --quiet http://www.cmake.org/CMakeDocs/cmake-mode.el \
	--output-document="$HOME/.emacs.d/emacs-cmake-project/cmake-mode.el"
fi

# Install xclip addon for yanking and pasting to and from the xclipboard.
if [ -d "$HOME/.emacs.d/xclip" ]; then
    (cd $HOME/.emacs.d/xclip && git pull --quiet)
else
    git clone https://github.com/emacsmirror/xclip.git $HOME/.emacs.d/xclip
fi

# Create vim data and plugin directories.
mkdir -p $HOME/.vim/
if [ ! -d "$HOME/.vim/autoload" ]; then
    git clone https://github.com/tpope/vim-pathogen.git /tmp/vim-pathogen
    cp -r /tmp/vim-pathogen/autoload $HOME/.vim/autoload
fi
ln -fs $UTILS_DIR/configs/vimrc $HOME/.vimrc

# Setup aspell configuration and additional dictionaries.
if [ ! -e $UTILS_DIR/configs/.dicts ]; then
    ln -fs $UTILS_DIR/configs/dicts $HOME/.dicts
fi

#TODO: Fix autostart of these
if [ -d "$XDG_CONFIG_HOME/autostart" ]; then
    echo "autostart exists."
    for file in $UTILS_DIR/autostart/*; do
	echo $file
    done

else
    echo "XDG_CONFIG_HOME propbably not set, or autostart does not exist."
fi

# Create a bashrc file with links to the script directories.
echo "# Don't edit this file, rerun install_utils.sh to update." > $HOME/.bashrc
echo "UTILS_DIR="${UTILS_DIR} >> $HOME/.bashrc
echo "source $UTILS_DIR/configs/bashrc" >> $HOME/.bashrc
