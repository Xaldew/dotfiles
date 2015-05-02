#!/usr/bin/env sh
# This script creates a folder for the development release of tmux,
# then compiles and installs it.

sudo apt-get --quiet=2 install libevent-dev libncurses5-dev
mkdir --parents $HOME/git/installs $HOME/.local
cd $HOME/git/installs

if [ -d $HOME/git/installs/emacs ]; then
    cd $HOME/git/installs/tmux
    make distclean
    git clean --force
    git pull
else
    git clone git://git.code.sf.net/p/tmux/tmux-code tmux
fi

cd $HOME/git/installs/tmux
sh autogen.sh
./configure --prefix=$HOME/.local --mandir=$HOME/.local/share/man
make -j
make install
