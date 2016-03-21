#!/usr/bin/env sh
# Install various external scripts from around the web.


# Install cloc.pl
url="http://sourceforge.net/projects/cloc/files/latest/download?source=files"
(
    wget --quiet $url --output-document=$local_prefix_dir/bin/cloc
    chmod u+x $local_prefix_dir/bin/cloc
) &

# Install bashmarks.sh
(
    tmp=$(mktemp --directory)
    cd $tmp
    git clone --quiet git://github.com/huyng/bashmarks.git
    cd bashmarks
    sed --in-place \
	"s|^\(INSTALL_DIR\s*=\s*\)~/.local/bin|\1${local_prefix_dir}/bin|g" \
	Makefile
    make install > /dev/null
) &

# Install git-prompt.sh.
base_url=https://raw.githubusercontent.com/git/git/master/contrib/completion/
(
    url=$base_url/git-prompt.sh
    wget --quiet $url --output-document=$local_prefix_dir/bin/git-prompt.sh
) &
(
    url=$base_url/git-completion.bash
    wget --quiet $url \
	 --output-document=$local_prefix_dir/bin/git-completion.bash
) &

# Install autoenv.
(
    url=git://github.com/kennethreitz/autoenv.git
    if [ ! -d $objects_dir/autoenv ]; then
        git clone --quiet $url $objects_dir/autoenv
    else
        cd $objects_dir/autoenv
        git pull
    fi
) &

# Install HG prompt.
if [ ! -d $objects_dir/hg-prompt ]; then
    hg --quiet clone \
       http://bitbucket.org/sjl/hg-prompt $objects_dir/hg-prompt
fi

# Download and install GDB STL beautifiers.
if [ ! -d $objects_dir/gdb_addons ]; then
    mkdir --parents $objects_dir/gdb_addons
    if [ ! -d $objects_dir/gdb_python_stl ]; then
	svn co svn://gcc.gnu.org/svn/gcc/trunk/libstdc++-v3/python \
	    $objects_dir/gdb_addons/gdb_python_stl
    fi
    url="https://sourceware.org/gdb/wiki/"
    url=$url"STLSupport?action=AttachFile&do=get&target=stl-views-1.0.3.gdb"
    if [ ! -d $objects_dir/gdb_addons/stl_views.gdb ]; then
	wget --quiet $url \
	     --output-document=$objects_dir/gdb_addons/stl_views.gdb
    fi
fi

# Install Leiningen for Clojure development.
if [ ! -x $local_prefix_dir/bin/lein ]; then
    url="https://raw.githubusercontent.com/technomancy/"
    url=$url"leiningen/stable/bin/lein"
    wget --quiet $url --output-document=$local_prefix_dir/bin/lein
    chmod u+x $local_prefix_dir/bin/lein
fi

# Install LanguageTool for spell- and grammarchecking.
if [ ! -d $local_prefix_dir/bin/languagetool ]; then
    dest=$local_prefix_dir/bin/languagetool
    mkdir --parents $dest
    tmpdir=$(mktemp --directory)
    url=https://languagetool.org/download/LanguageTool-stable.zip
    wget --quiet $url --output-document=$tmpdir/languagetool.zip
    zip=$tmpdir/languagetool.zip

    # Extract zip and strip toplevel directories.
    unzip -q -d "$dest" "$zip"
    for f in "$dest"/*;
    do
        mv $f/* "$dest"
        rmdir $f
    done
    rm -r $tmpdir
fi

# Install Emacs Cask.
if ! command -v cask >/dev/null 2>&1; then
    git clone --quiet https://github.com/cask/cask.git $objects_dir/cask
    $objects_dir/cask/bin/cask upgrade-cask
fi
