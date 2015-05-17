#### Install all main packages using the package manager.

# Install version control systems.
sudo yum --quiet --assumeyes install \
     git \
     bzr \
     subversion \
     mercurial \
     git-bzr

# Install editors and terminal mangement packages.
sudo yum --quiet --assumeyes install \
     vim \
     ssh \
     xclip

# Install compilers and coding utilities.
sudo yum --quiet --assumeyes install \
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
sudo yum --quiet --assumeyes install \
     python \
     python3 \
     python-pil \
     python-scipy \
     python-numpy \
     pylint

# Install OpenGL development utilities.
sudo yum --quiet --assumeyes install \
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
sudo yum --quiet --assumeyes install \
     texlive-full \
     doxygen-latex


# Install virtualization tools and cross compilers.
sudo yum --quiet --assumeyes install \
     qemu \
     gcc-arm-linux-gnueabihf \
     g++-arm-linux-gnueabihf \
     binutils-arm-linux-gnueabihf

# Install graphic design tools.
sudo yum --quiet --assumeyes install \
     imagemagick \
     inkscape \
     gimp

# Install miscellaneous packages.
sudo yum --quiet --assumeyes install \
     graphviz


# Install packages needed to compile tmux.
sudo yum --quiet --assumeyes install libevent-dev libncurses5-dev


# Install the packages needed to compile Emacs.
sudo yum --quiet --assumeyes install libmagickcore-dev libmagickwand-dev \
     libgtk-3-dev libjpeg-dev libgif-dev libtiff-dev
