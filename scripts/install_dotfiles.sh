#!/usr/bin/env bash
# Install links and files for configuring my configuration.

# Install all configuration files and plugins.
ln -fs $DOTFILES_DIR/configs/inputrc $HOME/.inputrc
ln -fs $DOTFILES_DIR/configs/bash_aliases $HOME/.bash_aliases

# Install git and setup user gitconfig and gitignore.
ln -fs $DOTFILES_DIR/configs/gitconfig $HOME/.gitconfig
ln -fs $DOTFILES_DIR/configs/gitignore $HOME/.gitignore

# Install tmux configuration.
ln -fs $DOTFILES_DIR/configs/tmux.conf $HOME/.tmux.conf

# Install latexmk configuration.
ln -fs $DOTFILES_DIR/configs/latexmkrc $HOME/.latexmkrc

# Install emacs configuration.
mkdir -p $HOME/.emacs.d
for elisp_file in $DOTFILES_DIR/configs/emacs.d/*.el
do
    ln -fs $elisp_file $HOME/.emacs.d/$(basename $elisp_file)
done
ln -fs $DOTFILES_DIR/configs/emacs.d/emacs $HOME/.emacs

# Create vim data and plugin directories.
mkdir -p $HOME/.vim/
if [ ! -d "$HOME/.vim/autoload" ]; then
    git clone https://github.com/tpope/vim-pathogen.git /tmp/vim-pathogen
    cp -r /tmp/vim-pathogen/autoload $HOME/.vim/autoload
fi
ln -fs $DOTFILES_DIR/configs/vimrc $HOME/.vimrc

# Setup aspell configuration and additional dictionaries.
if [ ! -e $DOTFILES_DIR/configs/.dicts ]; then
    rm -rf $HOME/.dicts
    ln -fs $DOTFILES_DIR/configs/dicts $HOME/.dicts
fi

# Change terminal colors to the solarized theme.
# git clone https://github.com/sgerrand/xfce4-terminal-colors-solarized.git \
#     /tmp/solarized-theme
# cp /tmp/solarized-theme/dark/terminalrc $HOME/.config/xfce4/terminal/terminalrc


# Create a bashrc file with links to the script directories.
echo "# Don't edit this file, rerun install_utils.sh to update." > $HOME/.bashrc
echo "DOTFILES_DIR="${DOTFILES_DIR} >> $HOME/.bashrc
echo "source \$DOTFILES_DIR/configs/bashrc" >> $HOME/.bashrc
echo "export PATH=\$DOTFILES_DIR/scripts:\$PATH" >> $HOME/.bashrc
