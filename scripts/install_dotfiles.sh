#!/usr/bin/env sh
# Install links and files for configuring my configuration.

# Create directories for local utilities.
mkdir --parents \
      $objects_dir \
      $local_prefix_dir/bin \
      $local_prefix_dir/share/man

# Install various external tools and utilities.
. $dotfiles_dir/scripts/install_external_scripts.sh


# Install all configuration files and plugins.
ln -fs $dotfiles_dir/configs/inputrc $HOME/.inputrc
ln -fs $dotfiles_dir/configs/bash_aliases $HOME/.bash_aliases


# Install git and setup user gitconfig and gitignore.
ln -fs $dotfiles_dir/configs/gitconfig $HOME/.gitconfig
ln -fs $dotfiles_dir/configs/gitignore $HOME/.gitignore


# Install Mercurial configurations.
ln -fs $dotfiles_dir/configs/hgrc $HOME/.hgrc


# Install GDB configurations.
ln -fs $dotfiles_dir/configs/gdbinit $HOME/.gdbinit


# Install tmux configuration and tmux plugin manager.
ln -fs $dotfiles_dir/configs/tmux.conf $HOME/.tmux.conf
if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
    git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
fi

# Install .screenrc.
ln -fs $dotfiles_dir/configs/screenrc $HOME/.screenrc
mkdir --parents $HOME/.screen/

# Install Xresources.
ln -fs $dotfiles_dir/configs/Xresources $HOME/.Xresources

# Install .ssh config.
mkdir --parents $HOME/.ssh
cp --force $dotfiles_dir/configs/ssh_config $HOME/.ssh/config
chmod 600 $HOME/.ssh/config

# Install latexmk configuration.
ln -fs $dotfiles_dir/configs/latexmkrc $HOME/.latexmkrc

# Install emacs configuration.
mkdir --parents $HOME/.emacs.d $HOME/.emacs.d/elisp/
for ef in $dotfiles_dir/configs/emacs.d/*.el
do
    ln -fs $ef $HOME/.emacs.d/$(basename $ef)
done
ln -fs $dotfiles_dir/configs/emacs.d/init.el $HOME/.emacs
rm -f $HOME/.emacs.d/snippets
ln -fs $dotfiles_dir/snippets $HOME/.emacs.d/snippets
for ef in $dotfiles_dir/configs/emacs.d/elisp/*.el
do
    ln -fs $ef $HOME/.emacs.d/elisp/$(basename $ef)
done
touch $HOME/.emacs.d/custom.el

# Download the gitolite-conf-mode file.
if [ ! -r "$HOME/.emacs.d/elisp/gl-conf-mode.el" ]; then
    tmp=$(mktemp --directory)
    git clone --quiet \
	git://github.com/llloret/gitolite-emacs.git $tmp/gitolite-emacs
    cp --force $tmp/gitolite-emacs/gl-conf-mode.el $HOME/.emacs.d/elisp/
    rm -rf $tmp
fi

# Create vim data and plugin directories.
mkdir --parents $HOME/.vim/
if [ ! -d "$HOME/.vim/autoload" ]; then
    tmp=$(mktemp --directory)
    git clone --quiet \
	https://github.com/tpope/vim-pathogen.git $tmp/vim-pathogen
    cp -r /tmp/vim-pathogen/autoload $HOME/.vim/autoload
fi
ln -fs $dotfiles_dir/configs/vimrc $HOME/.vimrc

# Setup aspell configuration and additional dictionaries.
rm -f $HOME/.dicts
ln -fs $dotfiles_dir/configs/dicts $HOME/.dicts


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
echo "# Don't edit this file, rerun install.sh to update." > $HOME/.zshenv
echo "ZDOTDIR=\$HOME/.zsh" >> $HOME/.zshenv
for k in ${!env[@]}; do
    e=${env[$k]}
    printf "export %s=%s\n" $k $e >> $HOME/.zshenv
done
echo "source \$dotfiles_dir/configs/zsh/zshenv" >> $HOME/.zshenv

# Install Prezto.
mkdir --parents $HOME/.zsh
if [ ! -d "$HOME/.zsh/.zprezto" ]; then
    git clone --quiet --recursive \
	https://github.com/sorin-ionescu/prezto.git \
	$HOME/.zsh/.zprezto
fi

# Create symlinks to configuration files.
for rc in $dotfiles_dir/configs/zsh/*;
do
    ln -fs "$rc" "$HOME/.zsh/.$(basename $rc)"
done
