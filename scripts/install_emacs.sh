#!/usr/bin/env sh
# Install the latest development release of Emacs.

# DEB: sudo apt-get install libmagickcore-dev libmagickwand-dev libgtk-3-dev
# libjpeg-dev libgif-dev libtiff-dev libxpm-dev librsvg2-dev libdbus-1-dev
# libgnutls-dev libxml2-dev
# RPM: sudo yum install libXpm-devel giflib-devel libtiff-devel

simple_conf=${1}

if [ ! -d $objects_dir/emacs ]; then
    git clone git://git.savannah.gnu.org/emacs.git $objects_dir/emacs
else
    cd $objects_dir/emacs
    git pull
fi

if [ "$OSTYPE" = "cygwin" ]; then
    conf=" --with-w32 "
elif [ -n "$simple_conf" ]; then
    conf=" --without-all "
else
    conf=""
fi


# Compile and install the latest master version of Emacs.
cd $objects_dir/emacs
make distclean
git clean --force -x -d

sh autogen.sh
./configure \
    --prefix="$local_prefix_dir" \
    --program-transform-name='s/^ctags$/ctags.emacs/' \
    $conf
make -j4
make install
