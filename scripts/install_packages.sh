#!/usr/bin/env bash
# This script install many of the software packages used for day to day
# development on linux computers.

source get_distribution.sh
dist=$(get_distribution | awk 'NR == 1 { print }')
packages=()
# Declare some useful functions.
############################################################
function is_installed ()
{
    status=1
    if [ dist == "ubuntu" -o dist == "debian" ]; then
	dpkg --status "$1" > /dev/null 2>&1
	status=$?
    elif [ dist == "fedora" ]; then
	if [ -z "$(rpm -qa | grep "$1")" ]; then
	    status=1
	else
	    status=0
	fi
    fi

    return $status
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
    if [ dist == "ubuntu" -o dist == "debian" ]; then
	sudo apt-get --quiet install ${packages[*]}
    elif [ dist == "fedora" ]; then
	sudo yum --quiet --assumeyes install ${packages[*]}
    fi
}
#############################################################

# Install version management
add_package git git-bzr subversion bzr mercurial

# Install editors and terminal multiplexers.
add_package tmux vim ssh xclip

# Install C/C++ Compilers, Python Interpreter, build tools.
add_package gcc g++ clang valgrind python pylint cmake

# Install GL development packages.
add_package freeglut3 freeglut3-dev libgl1-mesa-dev libglu1-mesa
add_package libglu1-mesa-dev libgl1-mesa-glx libx11-dev mesa-common-dev
add_package mesa-utils

# Install the Full latex-live distribution.
add_package texlive-full doxygen doxygen-latex auctex graphviz

# Install cross-compiling utilities for ARM development.
add_package qemu binutils-arm-linux-gnueabihf g++-arm-linux-gnueabihf
add_package gcc-arm-linux-gnueabihf

# Install the accumulated packages.
install_packages
