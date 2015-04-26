#!/usr/bin/env sh
# Install links and files for configuring my configuration.

# Install all configuration files and plugins.
ln -fs $DOTFILES_DIR/configs/inputrc $HOME/.inputrc
ln -fs $DOTFILES_DIR/configs/bash_aliases $HOME/.bash_aliases


# Install git and setup user gitconfig and gitignore.
ln -fs $DOTFILES_DIR/configs/gitconfig $HOME/.gitconfig
ln -fs $DOTFILES_DIR/configs/gitignore $HOME/.gitignore


# Install Mercurial configurations.
rm --force $HOME/.hgrc
(
    if [ ! -d $HOME/git/installs/hg-prompt ]; then
	hg --quiet clone \
	   http://bitbucket.org/sjl/hg-prompt $HOME/git/installs/hg-prompt
    fi
)
ln -fs $DOTFILES_DIR/configs/hgrc $HOME/.hgrc


# Install tmux configuration and tmux plugin manager.
ln -fs $DOTFILES_DIR/configs/tmux.conf $HOME/.tmux.conf
if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
    git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
fi

# Install .screenrc.
ln -fs $DOTFILES_DIR/configs/screenrc $HOME/.screenrc
mkdir --parents $HOME/.screen/

# Install Xresources.
ln -fs $DOTFILES_DIR/configs/Xresources $HOME/.Xresources

# Install .ssh config.
mkdir --parents $HOME/.ssh
cp --force $DOTFILES_DIR/configs/ssh_config $HOME/.ssh/config
chmod 600 $HOME/.ssh/config

# Install latexmk configuration.
ln -fs $DOTFILES_DIR/configs/latexmkrc $HOME/.latexmkrc

# Install emacs configuration.
mkdir --parents $HOME/.emacs.d $HOME/.emacs.d/elisp/
for ef in $DOTFILES_DIR/configs/emacs.d/*.el
do
    ln -fs $ef $HOME/.emacs.d/$(basename $ef)
done
ln -fs $DOTFILES_DIR/configs/emacs.d/emacs $HOME/.emacs
rm -f $HOME/.emacs.d/snippets
ln -fs $DOTFILES_DIR/snippets $HOME/.emacs.d/snippets
for ef in $DOTFILES_DIR/configs/emacs.d/elisp/*.el
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
fi

# Create vim data and plugin directories.
mkdir --parents $HOME/.vim/
if [ ! -d "$HOME/.vim/autoload" ]; then
    tmp=$(mktemp --directory)
    git clone --quiet \
	https://github.com/tpope/vim-pathogen.git $tmp/vim-pathogen
    cp -r /tmp/vim-pathogen/autoload $HOME/.vim/autoload
fi
ln -fs $DOTFILES_DIR/configs/vimrc $HOME/.vimrc

# Setup aspell configuration and additional dictionaries.
rm -f $HOME/.dicts
ln -fs $DOTFILES_DIR/configs/dicts $HOME/.dicts

# Create directories for local utilities.
mkdir --parents \
      $HOME/.local \
      $HOME/.local/bin \
      $HOME/.local/share/man

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

# Create a bashrc file with links to the script directories.
echo "# Don't edit this file, rerun install.sh to update." > $HOME/.bashrc
echo "DOTFILES_DIR="${DOTFILES_DIR} >> $HOME/.bashrc
echo "source \$DOTFILES_DIR/configs/bashrc" >> $HOME/.bashrc

# Create a profile file with links to the dotfile version.
echo "# Don't edit this file, rerun install.sh to update." > $HOME/.profile
echo "DOTFILES_DIR="${DOTFILES_DIR} >> $HOME/.profile
echo ". \$DOTFILES_DIR/configs/profile" >> $HOME/.profile

# Create a zshrc file with links to the script directories.
echo "# Don't edit this file, rerun install.sh to update." > $HOME/.zshenv
echo "DOTFILES_DIR="${DOTFILES_DIR} >> $HOME/.zshenv
echo "ZDOTDIR=\$HOME/.zsh" >> $HOME/.zshenv
echo "source \$DOTFILES_DIR/configs/zsh/zshenv" >> $HOME/.zshenv

# Install Prezto.
mkdir --parents $HOME/.zsh
if [ ! -d "$HOME/.zsh/.zprezto" ]; then
    git clone --quiet --recursive \
	https://github.com/sorin-ionescu/prezto.git \
	$HOME/.zsh/.zprezto
fi

# Create symlinks to configuration files.
for rc in $DOTFILES_DIR/configs/zsh/*;
do
    ln -fs "$rc" "$HOME/.zsh/.$(basename $rc)"
done
