#!/usr/bin/env bash
# This script install many of the software packages used for day to day
# development on linux computers.

source $DOTFILES_DIR/scripts/get_distribution.sh
dist=$(get_distribution | awk 'NR == 1 { print }')
if [ $dist == "ubuntu" -o $dist == "debian" ]; then
    source $DOTFILES_DIR/scripts/install_ubuntu_packages.sh
elif [ $dist == "fedora" ]; then
    source $DOTFILES_DIR/scripts/install_fedora_packages.sh
fi
