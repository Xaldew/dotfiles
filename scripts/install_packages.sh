#!/usr/bin/env sh
# This script install many of the software packages used for day to day
# development on linux computers.

. $dotfiles_dir/scripts/get_distribution.sh
dist=$(get_distribution | awk 'NR == 1 { print }')
if [ $dist == "ubuntu" -o $dist == "debian" ]; then
    . $dotfiles_dir/scripts/install_ubuntu_packages.sh
elif [ $dist == "fedora" ]; then
    . $dotfiles_dir/scripts/install_fedora_packages.sh
fi
