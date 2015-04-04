#!/usr/bin/env sh
# This script install packages used in my day to day work on Ubuntu and debian
# like distributions.

#### Install partner repositories
sudo add-apt-repository --yes ppa:xorg-edgers/ppa
sudo add-apt-repository --yes \
     "deb http://archive.canonical.com/ $(lsb_release -sc) partner"

sudo apt-get update

#### Install all main packages using the package manager.

# Install version control systems.
sudo apt-get --quiet=2 install \
     git \
     bzr \
     subversion \
     mercurial \
     git-bzr \
     git-cvs

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
     python-pip \
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
     remmina-plugin-vnc \
     remmina-plugin-gnome \
     remmina-plugin-nx \
     redshift \
     redshift-gtk \
     gphoto2 \
     darktable \
     deluge \
     nautilus-dropbox \
     steam \
     wine \
     skype \
     vlc \
     meld \
     baobab \
     smartmontools \
     gnome-disk-utility \
     unetbootin \
     gksu
