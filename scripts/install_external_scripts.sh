#!/usr/bin/env sh
# Install various external scripts from around the web.


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


# Install Prezto for zsh.
(
    if [ ! -d "$HOME/.zsh/.zprezto" ]; then
        git clone --quiet --recursive \
	    https://github.com/sorin-ionescu/prezto.git \
	    $HOME/.zsh/.zprezto
    fi
) &


# Install Bash-it.
(
    url=https://github.com/Bash-it/bash-it.git
    mkdir -p ${XDG_CONFIG_HOME}
    if [ ! -d ${XDG_CONFIG_HOME}/bash_it ]; then
        git clone --depth=1 ${url} ${XDG_CONFIG_HOME}/bash_it
        ${XDG_CONFIG_HOME}/bash_it/install.sh --no-modify-config --silent
        . ${XDG_CONFIG_HOME}/bash_it/bash_it.sh
        bash-it enable completion svn tmux makefile
        bash-it enable plugin dirs
    else
        cd ${XDG_CONFIG_HOME}/bash_it
        git pull
    fi
) &

# Install autoenv.
(
    url=https://github.com/hyperupcall/autoenv.git
    if [ ! -d $objects_dir/autoenv ]; then
        git clone --quiet $url $objects_dir/autoenv
    else
        cd $objects_dir/autoenv
        git pull
    fi
) &

# Install HG prompt.
if command -v hg > /dev/null 2>&1 && [ ! -d $objects_dir/hg-prompt ]; then
    url="http://bitbucket.org/sjl/hg-prompt"
    hg --quiet clone ${url} $objects_dir/hg-prompt
fi

# Download and install GDB STL beautifiers.
if command -v svn >/dev/null 2>&1 && [ ! -d $objects_dir/gdb_addons ]; then
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

# Install Emacs Cask.
if ! command -v cask >/dev/null 2>&1 && command -v emacs >/dev/null 2>&1; then
    git clone --quiet https://github.com/cask/cask.git $objects_dir/cask
    $objects_dir/cask/bin/cask upgrade-cask
fi
