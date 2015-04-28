#!/usr/bin/env sh
# Install various external scripts from around the web.


# Install cloc.pl
url="http://sourceforge.net/projects/cloc/files/latest/download?source=files"
(
    wget $url --output-document=$HOME/.local/bin/cloc.pl --quiet
    chmod u+x $HOME/.local/bin/cloc.pl
) &

# Install bashmarks.sh
(
    tmp=$(mktemp --directory)
    cd $tmp
    git clone --quiet git://github.com/huyng/bashmarks.git
    cd bashmarks
    make install > /dev/null
) &

# Install git-prompt.sh.
base_url=https://raw.githubusercontent.com/git/git/master/contrib/completion/
(
    url=$base_url/git-prompt.sh
    wget $url --output-document=$HOME/.local/bin/git-prompt.sh --quiet
) &
(
    url=$base_url/git-completion.bash
    wget $url --output-document=$HOME/.local/bin/git-completion.bash --quiet
) &
