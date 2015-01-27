#!/usr/bin/env sh
# Install the latest development relase of Emacs.

sudo apt-get --quiet=2 install \
     libmagickcore-dev \
     libmagickwand-dev \
     libgtk-3-dev \
     libjpeg-dev \
     libgif-dev \
     libtiff-dev \
     libxpm-dev

mkdir --parents $HOME/git/installs $HOME/.local
cd $HOME/git/installs

if [ -d $HOME/git/installs/emacs ]; then
    cd $HOME/git/installs/emacs
    git pull
else
    git clone git://git.savannah.gnu.org/emacs.git emacs
fi

cd $HOME/git/installs/emacs
sh autogen.sh
./configure --prefix=$HOME/.local --enable-link-time-optimization --with-imagemagick
make -j
make install
