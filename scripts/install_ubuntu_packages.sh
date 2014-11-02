#!/usr/bin/env sh
# This script install packages used in my day to day work on Ubuntu and debian
# like distributions.


#### Install all main packages using the package manager.

# Install version control systems.
sudo apt-get install \
     git \
     bzr \
     subversion \
     mercurial \
     git-bzr

# Install editors and terminal mangement packages.
sudo apt-get install \
     vim \
     ssh \
     xclip

# Install compilers and coding utilities.
sudo apt-get install \
     build-essential \
     gcc \
     g++ \
     clang \
     valgrind \
     automake \
     autoconf \
     cmake \
     doxygen \
     ccache \
     global \
     silversearcher-ag

# Install Python and utilities.
sudo apt-get install \
     python \
     python3 \
     python-pil \
     python-scipy \
     python-numpy \
     pylint

# Install OpenGL development utilities.
sudo apt-get install \
     freeglut3 \
     freeglut3-dev \
     libgl1-mesa-dev \
     libglu1-mesa \
     libglu1-mesa-dev \
     libgl1-mesa-glx \
     libx11-dev \
     mesa-common-dev \
     mesa-utils

# Install latex and documentation utilities.
sudo apt-get install \
     texlive-full \
     doxygen-latex


# Install virtualization tools and cross compilers.
sudo apt-get Install \
     qemu \
     gcc-arm-linux-gnueabihf \
     g++-arm-linux-gnueabihf \
     binutils-arm-linux-gnueabihf

# Install graphic design tools.
sudo apt-get install \
     imagemagick \
     inkscape \
     gimp

# Install miscellaneous packages.
sudo apt-get install \
     graphviz


# Install latest version of Tmux.
sudo apt-get install libevent-dev libncurses5-dev
tmp_dir=$(mktemp --directory)
trap "{ cd - ; rm -rf $tmp_dir; exit 255; }" SIGINT

cd $tmp_dir
git clone git://git.code.sf.net/p/tmux/tmux-code tmux
cd tmux
sh autogen.sh
./configure
make -j
sudo make install


# Install the latest version of Emacs.
sudo apt-get install libmagickcore-dev libmagickwand-dev \
     libgtk-3-dev libjpeg-dev libgif-dev libtiff-dev
