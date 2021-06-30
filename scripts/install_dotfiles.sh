#!/usr/bin/env sh
# Install links and files for configuring my configuration.

# Create directories for local utilities.
mkdir --parents \
      $objects_dir \
      $local_prefix_dir/bin \
      $local_prefix_dir/share/man


file_contains_line()
{
    line=${1:?"Missing string to search for."}
    file=${2:?"Missing file to search in."}
    grep -Fxq -e "${line}" -f ${file}
}


add_line_if_missing()
{
    line=${1:?"Missing string to search for."}
    file=${2:?"Missing file to search in."}
    output=${3:-${file}}
    if ! file_contains_line "${line}" "${file}"; then
        printf "${line}\n" >> ${output}
    fi
}


add_supported_ssh_config()
{
    # Check for duplicates and unsupported options.
    tmp=$(mktemp)
    head=$(mktemp)
    while IFS='' read -r line
    do
        add_line_if_missing "$line" "$HOME/.ssh/config" "$tmp"
        case $(ssh -F $tmp _ 2>&1)
        in
            *"Bad configuration option:"*)
                # Unsupported option remove the line.
                head -n -1 $tmp > $head && cp $head $tmp
                ;;
            *"Name or service not known"*)
                # Supported option.
                ;;
        esac
    done < $dotfiles_dir/configs/ssh_config

    cat $tmp >> $HOME/.ssh/config
    rm -r $tmp $head
}


create_linkfarm()
{
    # Create a recrusive linkfarm.
    local src=${1:?"Missing source directory."}
    local dst=${2:?"Missing destination directory."}
    local f=""
    mkdir --parents ${dst}
    for f in ${src}/* ${src}/.[!.]* ${src}/..?*;
    do
        if [ -d $f ]; then create_linkfarm $f $dst/$(basename $f) ; fi
        if [ -f $f ]; then ln -fs -T $f $dst/$(basename $f)       ; fi
    done
}


# Install common ssh configuration.
mkdir --parents $HOME/.ssh
add_supported_ssh_config
if command -v xfconf-query 1> /dev/null 2> /dev/null ; then
    # Disable default SSH agent.
    xfconf-query -c xfce4-session -p /startup/ssh-agent/enabled -n -t bool -s false
fi
printf "use-standard-socket\n" >> ~/.gnupg/gpg-agent.conf
printf "enable-ssh-support\n" >> ~/.gnupg/gpg-agent.conf

# Install all configuration files and plugins.
ln -fs $dotfiles_dir/configs/inputrc $HOME/.inputrc
ln -fs $dotfiles_dir/configs/bash_aliases $HOME/.bash_aliases
ln -fs $dotfiles_dir/configs/bash_profile $HOME/.bash_profile
ln -fs $dotfiles_dir/configs/bash_completion $HOME/.bash_completion
mkdir -p $HOME/.bash_completion.d


# Install git and setup user gitconfig and gitignore.
ln -fs $dotfiles_dir/configs/gitconfig $HOME/.gitconfig
ln -fs $dotfiles_dir/configs/gitignore $HOME/.gitignore
ln -fs $dotfiles_dir/configs/gitattributes $HOME/.gitattributes

# Install Mercurial configurations.
ln -fs $dotfiles_dir/configs/hgrc $HOME/.hgrc


# Install GDB configurations.
ln -fs $dotfiles_dir/configs/gdbinit $HOME/.gdbinit


# Install tmux configuration and tmux plugin manager.
mkdir -p $XDG_CONFIG_HOME/tmux
create_linkfarm $dotfiles_dir/configs/tmux $XDG_CONFIG_HOME/tmux
if [ ! -d "$XDG_CONFIG_HOME/tmux/plugins/tpm" ]; then
    git clone https://github.com/tmux-plugins/tpm $XDG_CONFIG_HOME/tmux/plugins/tpm
fi

# Install .screenrc.
ln -fs $dotfiles_dir/configs/screenrc $HOME/.screenrc
mkdir --parents $HOME/.screen/

# Install Xresources.
ln -fs $dotfiles_dir/configs/Xresources $HOME/.Xresources

# Install latexmk configuration.
ln -fs $dotfiles_dir/configs/latexmkrc $HOME/.latexmkrc

# Install emacs configuration.
create_linkfarm $dotfiles_dir/configs/emacs.d $HOME/.emacs.d
touch $HOME/.emacs.d/custom.el

# Create vim data and plugin directories.
mkdir --parents $HOME/.vim/
if [ ! -d "$HOME/.vim/autoload" ]; then
    tmp=$(mktemp --directory)
    git clone --quiet \
	https://github.com/tpope/vim-pathogen.git $tmp/vim-pathogen
    cp -r $tmp/vim-pathogen/autoload $HOME/.vim/autoload
    rm -rf $tmp
fi
ln -fs $dotfiles_dir/configs/vimrc $HOME/.vimrc

# Setup aspell configuration and additional dictionaries.
ln -fs -T $dotfiles_dir/configs/dicts $HOME/.dicts


# Create a bashrc file with links to the script directories.
echo "# Don't edit this file, rerun install.sh to update." > $HOME/.bashrc
for k in ${!env[@]}; do
    e=${env[$k]}
    printf "export %s=%s\n" $k $e >> $HOME/.bashrc
done
echo "source \$dotfiles_dir/configs/bashrc" >> $HOME/.bashrc


# Create a profile file with links to the dotfile version.
echo "# Don't edit this file, rerun install.sh to update." > $HOME/.profile
for k in ${!env[@]}; do
    e=${env[$k]}
    printf "export %s=%s\n" $k $e >> $HOME/.profile
done
echo ". \$dotfiles_dir/configs/profile" >> $HOME/.profile


# Create a zshrc file with links to the script directories.
mkdir --parents $HOME/.zsh
echo "# Don't edit this file, rerun install.sh to update." > $HOME/.zshenv
echo "ZDOTDIR=\$HOME/.zsh" >> $HOME/.zshenv
for k in ${!env[@]}; do
    e=${env[$k]}
    printf "export %s=%s\n" $k $e >> $HOME/.zshenv
done
echo "source \$dotfiles_dir/configs/zsh/zshenv" >> $HOME/.zshenv

# Create symlinks to configuration files.
for rc in $dotfiles_dir/configs/zsh/*;
do
    ln -fs "$rc" "$HOME/.zsh/.$(basename $rc)"
done
