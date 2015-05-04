#!/usr/bin/env sh
# Install the latest development release of Emacs.

sudo apt-get --quiet=2 install \
     libmagickcore-dev \
     libmagickwand-dev \
     libgtk-3-dev \
     libjpeg-dev \
     libgif-dev \
     libtiff-dev \
     libxpm-dev \
     libxaw7-dev

mkdir --parents $HOME/git/installs
cd $HOME/git/installs

if [ -d $HOME/git/installs/emacs ]; then
    cd $HOME/git/installs/emacs
    make distclean
    git clean --force
    git checkout -B master remotes/origin/master
    git pull
    git checkout -B emacs-24 remotes/origin/emacs-24
    git pull
else
    git clone git://git.savannah.gnu.org/emacs.git emacs
fi

# Compile and install Emacs-24.
cd $HOME/git/installs/emacs
git checkout -B emacs-24 origin/emacs-24
sh autogen.sh
./configure --prefix=$HOME/.local \
	    --mandir=$HOME/.local/share/man \
	    --enable-link-time-optimization \
	    --with-imagemagick \
	    --with-x-toolkit=lucid
make -j
make install


# Compile and install the latest Emacs.
make distclean
git clean --force
git checkout -B master origin/master
sh autogen.sh
./configure --prefix=$HOME/.local \
	    --mandir=$HOME/.local/share/man \
	    --enable-link-time-optimization \
	    --with-imagemagick \
	    --with-x-toolkit=lucid
make -j
make install
