#!/usr/bin/env sh
# Install the latest development release of Emacs.

sudo apt-get --quiet=2 install \
     libmagickcore-dev \
     libmagickwand-dev \
     libgtk-3-dev \
     libjpeg-dev \
     libgif-dev \
     libtiff-dev \
     libxpm-dev \
     libxaw7-dev

cd $objects_dir

if [ -d $objects_dir/emacs ]; then
    cd $objects_dir/emacs
    make distclean
    git clean --force
    find -name *.elc -type f -print | xargs rm --force
    find -name *.loaddefs.el? -type f -print | xargs rm --force
    git checkout -B master remotes/origin/master
    git pull
    git checkout -B emacs-24 remotes/origin/emacs-24
    git pull
else
    git clone git://git.savannah.gnu.org/emacs.git emacs
fi

# Compile and install Emacs-24.
cd $objects_dir/emacs
git checkout -B emacs-24 origin/emacs-24
sh autogen.sh
./configure --prefix=$local_prefix_dir \
	    --mandir=$local_prefix_dir/share/man \
	    --enable-link-time-optimization \
	    --with-imagemagick \
	    --with-x-toolkit=lucid
make -j4
make install


# Compile and install the latest Emacs.
make distclean
git clean --force
find -name *.elc -type f -print | xargs rm --force
find -name *.loaddefs.el? -type f -print | xargs rm --force
git checkout -B master origin/master
sh autogen.sh
./configure --prefix=$local_prefix_dir \
	    --mandir=$local_prefix_dir/share/man \
	    --enable-link-time-optimization \
	    --with-imagemagick \
	    --with-x-toolkit=lucid
make -j4
make install
