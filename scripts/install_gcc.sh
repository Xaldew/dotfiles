#!/usr/bin/env sh
# Install the GNU Compiler Collection.

sudo apt-get install \
     libgmp-dev \
     libmpfr-dev \
     libmpc-dev

cd $HOME/git/installs

if [ -d $HOME/git/installs/gcc ]; then
    cd $HOME/git/installs/gcc
    git pull
else
    git clone git://gcc.gnu.org/git/gcc.git
fi

cd $HOME/git/installs/emacs
sh autogen.sh
./configure --prefix=$HOME/.local \
	    --mandir=$HOME/.local/share/man \
	    --enable-link-time-optimization \
	    --with-imagemagick \
	    --with-x-toolkit=lucid
make -j
make install
