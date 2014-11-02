#!/usr/bin/env sh
# This script install packages used in my day to day work on Ubuntu and debian
# like distributions.


# Install all main packages using the package manager.
sudo apt-get install \
    git \
    bzr \
    subversion \
    mercurial \
    git-bzr \
    \
    vim \
    ssh \
    xclip \
    \
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
    silversearcher-ag \
    \
    python \
    pylint \
    \
    freeglut3 \
    freeglut3-dev \
    libgl1-mesa-dev \
    libglu1-mesa \
    libglu1-mesa-dev \
    libgl1-mesa-glx \
    libx11-dev \
    mesa-common-dev \
    mesa-utils \
    \
    texlive-full \
    doxygen-latex \
    \
    graphviz \
    \
    qemu \
    gcc-arm-linux-gnueabihf \
    g++-arm-linux-gnueabihf \
    binutils-arm-linux-gnueabihf \
    \
    imagemagick \
    inkscape \
    gimp \


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
