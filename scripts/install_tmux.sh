#!/usr/bin/env sh
# This script creates a folder for the development release of tmux,
# then compiles and installs it.

sudo apt-get --quiet=2 install libevent-dev libncurses5-dev
mkdir -p $HOME/git/installs
cd $HOME/git/installs

if [ -d $HOME/git/installs/emacs ]; then
    git clone git://git.code.sf.net/p/tmux/tmux-code tmux
else
    cd $HOME/git/installs/tmux
    git pull
fi

cd $HOME/git/installs/tmux
sh autogen.sh
./configure
make -j
sudo make install
