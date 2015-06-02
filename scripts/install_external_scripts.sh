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

# Install HG prompt.
if [ ! -d $objects_dir/hg-prompt ]; then
    hg --quiet clone \
       http://bitbucket.org/sjl/hg-prompt $objects_dir/hg-prompt
fi
