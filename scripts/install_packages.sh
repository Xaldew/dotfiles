#!/usr/bin/env bash
# This script install many of the software packages used for day to day
# development on linux computers.
# @TODO: Make this script also work on RPM based distributions.

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
