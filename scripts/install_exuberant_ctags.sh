#!/usr/bin/env sh
# Install latest Exuberant ctags.

cd $HOME/git/installs/

if [ -d $HOME/git/installs/exuberant-ctags ]; then
    cd $HOME/git/installs/exuberant-ctags
    make distclean
    git reset --hard
    git clean --force
    git svn rebase
else
    git svn clone \
	svn://svn.code.sf.net/p/ctags/code/trunk exuberant-ctags
fi

cd $HOME/git/installs/exuberant-ctags

sed --in-place 's|^\(CTAGS_PROG\s*=\s*\)ctags|\1 ex-ctags|g' Makefile.in
sed --in-place 's|^\(ETAGS_PROG\s*=\s*\)etags|\1 ex-etags|g' Makefile.in
autoreconf
./configure \
    --prefix=$HOME/.local \
    --mandir=$HOME/.local/share/man \
    --disable-etags
make -j4
make install
