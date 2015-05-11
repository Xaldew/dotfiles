#!/usr/bin/env sh
# Install the latest development relase of Emacs.

sudo apt-get --quiet=2 install \
     bison \
     flex \
     gperf

cd $HOME/git/installs
CVSROOT=:pserver:anonymous@cvs.savannah.gnu.org:/sources/global
if [ -d $HOME/git/installs/global ]; then
    cd $HOME/git/installs/global
    git cvsimport -d $CVSROOT -r cvs -v -R global \
	-A $HOME/git/installs/global/.cvs_authors
else
    mkdir -p $HOME/git/installs/global
    touch $HOME/git/installs/global/.cvs_authors
    git cvsimport -p "-x" -d $CVSROOT -C global -r cvs -k -z3 -v \
	-A $HOME/git/installs/global/.cvs_authors -R global
fi

cd $HOME/git/installs/global
sh reconf.sh
./configure \
    --prefix=$HOME/.local \
    --mandir=$HOME/.local/share/man \
    --with-exuberant-ctags=ex-ctags

make -j
make install
