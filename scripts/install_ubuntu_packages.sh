#!/usr/bin/env sh
# This script install packages used in my day to day work on Ubuntu and debian
# like distributions.


#### Install all main packages using the package manager.

# Install version control systems.
sudo apt-get --quiet=2 install \
    git \
    bzr \
    subversion \
    mercurial \
    git-bzr

# Install editors and terminal mangement packages.
sudo apt-get --quiet=2 install \
    vim \
    ssh \
    xclip

# Install compilers and coding utilities.
sudo apt-get --quiet=2 install \
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
sudo apt-get --quiet=2 install \
    python \
    python3 \
    python-pil \
    python-scipy \
    python-numpy \
    python-pygments \
    python3-pygments \
    pylint

# Install OpenGL development utilities.
sudo apt-get --quiet=2 install \
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
sudo apt-get --quiet=2 install \
    texlive-full \
    doxygen-latex


# Install virtualization tools and cross compilers.
sudo apt-get --quiet=2 install \
    qemu \
    gcc-arm-linux-gnueabihf \
    g++-arm-linux-gnueabihf \
    binutils-arm-linux-gnueabihf

# Install graphic design tools.
sudo apt-get --quiet=2 install \
    imagemagick \
    inkscape \
    gimp

# Install miscellaneous packages.
sudo apt-get --quiet=2 install \
    graphviz \
    remmina \
    remmina-plugins-nx \
    redshift \
    redshift-gtk


# Install latest version of Tmux.
sudo apt-get --quiet=2 install libevent-dev libncurses5-dev
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
sudo apt-get --quiet=2 install libmagickcore-dev libmagickwand-dev \
    libgtk-3-dev libjpeg-dev libgif-dev libtiff-dev
cd $tmp_dir
git clone git://git.savannah.gnu.org/emacs.git
sh autogen.sh
./configure --enable-link-time-optimization --with-imagemagick
make -j
sudo make install