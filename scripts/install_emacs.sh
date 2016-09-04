#!/usr/bin/env sh
# Install the latest development release of Emacs.

# DEB: sudo apt-get install libmagickcore-dev libmagickwand-dev libgtk-3-dev
# libjpeg-dev libgif-dev libtiff-dev libxpm-dev librsvg2-dev libdbus-1-dev
# libgnutls-dev libxml2-dev


if [ ! -d $objects_dir/emacs ]; then
    git clone git://git.savannah.gnu.org/emacs.git $objects_dir/emacs
else
    cd $objects_dir/emacs
    git pull
fi

if [ "$OSTYPE" = "cygwin" ]; then
    conf=" --with-w32 "
else
    conf=""
fi


# Compile and install the latest version of Emacs-25.
cd $objects_dir/emacs
make distclean
git clean --force

# To do a full clean-up, uncomment the following lines.
# find -name *.elc -type f -print | xargs rm --force
# find -name *.loaddefs.el* -type f -print | xargs rm --force

git checkout -B emacs-25 origin/emacs-25
sh autogen.sh
./configure --prefix="$local_prefix_dir" $conf
make -j4
make install
