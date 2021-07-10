#!/usr/bin/env sh
# Install links and files for configuring my configuration.

dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)
. ${dir}/scripts/install_utils.sh

# Create directories for local utilities.
mkdir -p \
      $objects_dir \
      $local_prefix_dir/bin \
      $local_prefix_dir/share/man

# Install common ssh configuration.
mkdir -p $HOME/.ssh
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

# Install GDB configurations.
ln -fs $dotfiles_dir/configs/gdbinit $HOME/.gdbinit

# Install .screenrc.
mkdir -p $HOME/.screen/
ln -fs $dotfiles_dir/configs/screenrc $HOME/.screenrc

# Install latexmk configuration.
ln -fs $dotfiles_dir/configs/latexmkrc $HOME/.latexmkrc

# Install emacs configuration.
create_linkfarm $dotfiles_dir/configs/emacs.d $HOME/.emacs.d
touch $HOME/.emacs.d/custom.el

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
