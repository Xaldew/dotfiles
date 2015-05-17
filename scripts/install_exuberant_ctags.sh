#!/usr/bin/env sh
# Install latest Exuberant ctags and rename it appropriately.

cd $objects_dir
if [ -d $objects_dir/exuberant-ctags ]; then
    cd $objects_dir/exuberant-ctags
    make distclean
    git reset --hard
    git clean --force
    git svn rebase
else
    git svn clone \
	svn://svn.code.sf.net/p/ctags/code/trunk exuberant-ctags
fi

cd $objects_dir/exuberant-ctags

sed --in-place 's|^\(CTAGS_PROG\s*=\s*\)ctags|\1 ex-ctags|g' Makefile.in
sed --in-place 's|^\(ETAGS_PROG\s*=\s*\)etags|\1 ex-etags|g' Makefile.in
autoreconf
./configure \
    --prefix=$local_prefix_dir \
    --mandir=$local_prefix_dir/share/man \
    --disable-etags
make -j4
make install
