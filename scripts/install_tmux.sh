#!/usr/bin/env sh
# This script creates a folder for the development release of tmux,
# then compiles and installs it.

sudo apt-get --quiet=2 install libevent-dev libncurses5-dev
mkdir -p $HOME/git/installs
cd $HOME/git/installs

if [ -d $HOME/git/installs/emacs ]; then
    cd $HOME/git/installs/tmux
    git pull
else
    git clone git://git.code.sf.net/p/tmux/tmux-code tmux
fi

cd $HOME/git/installs/tmux
sh autogen.sh
./configure
make -j
sudo make install
