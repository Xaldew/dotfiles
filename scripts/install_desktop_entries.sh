#!/usr/bin/env bash
# Install .desktop application entries to allow Window managers to find them.

# Figure out where we have placed the script.
install_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
dotfiles_dir=${install_dir}/..
bindir=$(cd ${XDG_DATA_HOME}/../bin ; pwd)

# Requires sudo, so lets not use it.
# sudo update-alternatives \
#     --install ${bindir}/ultimaker-cura \
#     ultimaker-cura \
#     ${bindir}/Ultimaker_Cura-4.13.1.AppImage \
#     1

# TODO: Automatic way of finding updated binaries.
ln -f -s ${bindir}/Ultimaker_Cura-4.13.1.AppImage ${bindir}/ultimaker-cura


# See https://specifications.freedesktop.org/menu-spec/menu-spec-1.0.html#category-registry
# for more details about categories.


# Install all the desktop files.

# The Exec path must be absolute, so make sure to expand all variables in the
# desktop entries
for f in ${dotfiles_dir}/linux/*.desktop; do
    bindir=${bindir} envsubst < $f > $XDG_DATA_HOME/applications/$(basename $f)
done
