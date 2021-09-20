#!/usr/bin/env sh
# Install links and files for configuring my XDG configuration.

dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)
. ${dir}/scripts/install_utils.sh

# Create directories for local utilities.
mkdir -p \
      $objects_dir \
      $local_prefix_dir/bin \
      $local_prefix_dir/share/man \
      "$XDG_CONFIG_HOME"/aspell \
      "$XDG_CONFIG_HOME"/emacs \
      "$XDG_CONFIG_HOME"/git \
      "$XDG_CONFIG_HOME"/gdb \
      "$XDG_CONFIG_HOME"/gnupg \
      "$XDG_CONFIG_HOME"/hg \
      "$XDG_CONFIG_HOME"/latexmk \
      "$XDG_CONFIG_HOME"/readline \
      "$XDG_CONFIG_HOME"/screen \
      "$XDG_CONFIG_HOME"/tmux \
      "$XDG_CONFIG_HOME"/tmux/plugins \
      "$XDG_CONFIG_HOME"/zsh \
      "$XDG_CONFIG_HOME"/vim

# Install common ssh configuration.
printf "use-standard-socket\n" >> "$XDG_CONFIG_HOME"/gnupg/gpg-agent.conf
printf "enable-ssh-support\n"  >> "$XDG_CONFIG_HOME"/gnupg/gpg-agent.conf

# Install git and setup user gitconfig and gitignore.
ln -fs $dotfiles_dir/configs/gitconfig "$XDG_CONFIG_HOME"/git/config
ln -fs $dotfiles_dir/configs/gitignore "$XDG_CONFIG_HOME"/git/ignore
ln -fs $dotfiles_dir/configs/gitignore "$XDG_CONFIG_HOME"/git/attributes

# Install Mercurial configurations.
ln -fs $dotfiles_dir/configs/hgrc "$XDG_CONFIG_HOME"/hg/hgrc

# Install GDB configurations.
ln -fs $dotfiles_dir/configs/gdbinit "$XDG_CONFIG_HOME"/gdb/init

# Install tmux configuration and tmux plugin manager.
create_linkfarm $dotfiles_dir/configs/tmux "$XDG_CONFIG_HOME"/tmux
if [ ! -d "$XDG_CONFIG_HOME/tmux/plugins/tpm" ]; then
    git clone https://github.com/tmux-plugins/tpm "$XDG_CONFIG_HOME"/tmux/plugins/tpm
fi

# Install screenrc.
ln -fs $dotfiles_dir/configs/screenrc "$XDG_CONFIG_HOME"/screen/screenrc

# Install latexmk configuration.
ln -fs $dotfiles_dir/configs/latexmkrc "$XDG_CONFIG_HOME"/latexmk/latexmkrc

# Install emacs configuration.
create_linkfarm $dotfiles_dir/configs/emacs.d "$XDG_CONFIG_HOME"/emacs
touch "$XDG_CONFIG_HOME"/emacs/custom.el

# Create vim data and plugin directories.
ln -fs $dotfiles_dir/configs/vimrc "$XDG_CONFIG_HOME"/vim/vimrc

# GNU Readline configuration.
ln -fs $dotfiles_dir/configs/inputrc "$XDG_CONFIG_HOME"/readline/inputrc

# Install all Bash config files.
# Note: Bash is not XDG compliant.
ln -fs $dotfiles_dir/configs/bash_aliases $HOME/.bash_aliases
ln -fs $dotfiles_dir/configs/bash_profile $HOME/.bash_profile
ln -fs $dotfiles_dir/configs/bash_completion $HOME/.bash_completion
mkdir -p $HOME/.bash_completion.d

# Create a bashrc file with links to the script directories.
echo "# Don't edit this file, rerun install.sh to update." > $HOME/.bashrc
for k in ${!env[@]}; do
    if [ "$k" = "PROMPT_COMMAND" ]; then
        continue
    fi
    e=${env[$k]}
    printf "export %s=%s\n" $k $e >> $HOME/.bashrc
done
echo "source \$dotfiles_dir/configs/bashrc" >> $HOME/.bashrc


# Create a profile file with links to the dotfile version.
echo "# Don't edit this file, rerun install.sh to update." > $HOME/.profile
for k in ${!env[@]}; do
    if [ "$k" = "PROMPT_COMMAND" ]; then
        continue
    fi
    e=${env[$k]}
    printf "export %s=%s\n" $k $e >> $HOME/.profile
done
echo ". \$dotfiles_dir/configs/profile" >> $HOME/.profile


# Create a zshrc file with links to the script directories.
mkdir -p "$XDG_CONFIG_HOME"/zsh
echo "# Don't edit this file, rerun install.sh to update." > $HOME/.zshenv
echo "ZDOTDIR=$XDG_CONFIG_HOME/zsh" >> $HOME/.zshenv
for k in ${!env[@]}; do
    if [ "$k" = "PROMPT_COMMAND" ]; then
        continue
    fi
    e=${env[$k]}
    printf "export %s=%s\n" $k $e >> $HOME/.zshenv
done
echo "source \$dotfiles_dir/configs/zsh/zshenv" >> $HOME/.zshenv

# Create symlinks to configuration files.
for rc in $dotfiles_dir/configs/zsh/*;
do
    ln -fs "$rc" "$XDG_CONFIG_HOME/zsh/.$(basename $rc)"
done
