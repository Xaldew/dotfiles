#!/usr/bin/env sh
# Install the latest development relase of Emacs.

sudo apt-get --quiet=2 install \
     bison \
     flex \
     gperf

cd $objects_dir
CVSROOT=:pserver:anonymous@cvs.savannah.gnu.org:/sources/global
if [ -d $objects_dir/global ]; then
    cd $objects_dir/global
    git cvsimport -d $CVSROOT -r cvs -v -R global \
	-A $objects_dir/global/.cvs_authors
else
    mkdir -p $objects_dir/global
    touch $objects_dir/global/.cvs_authors
    git cvsimport -p "-x" -d $CVSROOT -C global -r cvs -k -z3 -v \
	-A $objects_dir/global/.cvs_authors -R global
fi

cd $objects_dir/global
sh reconf.sh
./configure \
    --prefix=$local_prefix_dir \
    --mandir=$local_prefix_dir/share/man \
    --with-exuberant-ctags=ex-ctags
make -j4
make install
