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

mkdir -p $HOME/git/installs
cd $HOME/git/installs

if [ -d $HOME/git/installs/emacs ]; then
    git clone git://git.savannah.gnu.org/emacs.git emacs
else
    cd $HOME/git/installs/emacs
    git pull
fi

cd $HOME/git/installs/emacs
sh autogen.sh
./configure --enable-link-time-optimization --with-imagemagick
make -j
sudo make install
