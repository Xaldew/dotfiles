#!/usr/bin/env bash

# Source common utilities.
source $dotfiles_dir/scripts/get_distribution.sh
source $dotfiles_dir/configs/common.sh

# Initialize tool to load modules.
my_locale=$(locale)
source /arm/tools/setup/init/bash
source <(printf "${my_locale}\n")

# Exports
export ARM_PROJECT_NR=PJ02470
export SCRATCH="/arm/scratch/$USER"
export GIT_HOME_DIR="$HOME/git"

# bjobs/bwhat to monitor queue.
alias bi="bsub -Is -P $ARM_PROJECT_NR -R 'rhe6 && os64' bash"
alias bs='bsub -P $ARM_PROJECT_NR -q normal -R "rhe6 && os64"'
alias bissh="ssh lun-login2.lund.arm.com"


# Various utility functions.

set_arm_gitconfig()
{
    # Set the ARM gitconfig for gerrit repos. Otherwise, commits cannot be
    # pushed.
    for arg in "$@";
    do
	(cd $arg && git config user.email "gustaf.waldemarson@arm.com")
    done
}

my_module()
{
    # Create an overload of the ARM module command to adjust /etc/lsb-release.
    if [ "$(get_dist -i)" = "ubuntu" -a "$(get_dist -r)" = "18.04" ]; then
        my_lsb=$(cat /etc/lsb-release)
        lsb=$(sed 's/18.04/16.04/' /etc/lsb-release)
        printf "${lsb}\n" > /etc/lsb-release
        module "$@" 2>/dev/null
        printf "${my_lsb}\n" > /etc/lsb-release
    else
        module "$@"
    fi
}

module_setup()
{
    # Modifies the environment if necessary for easier use of the module system.
    # Might require the use of sudo.
    if [ "$(get_dist -i)" = "ubuntu" -a "$(get_dist -r)" = "18.04" ]; then
        sudo chmod 664 /etc/lsb-release
        sudo chgrp ${USER} /etc/lsb-release
    fi
}

setenv()
{
    # (t)csh setenv compatibility.
    export "$1"="$@"
}

# Load some basic modules.
if [ "`get_dist -i`" = "ubuntu" ]; then

    # This environment variable unfortunately doesn't work, since IT haven't
    # added support for Ubuntu to the emulated platforms.
    #export DEPOT_EMULATED_PLATFORM=ubu-16.04
    my_module load swdev arm/cluster

    # Needed by blueprint.
    my_module load google/golang ninja-build/ninja gnu/gcc/4.9.1_lto gnu/cmake/3.10.2 python/ply_py2.7.8 
else
    # Running CentOS. Load CentOS specific packages.

    # Load tmux on the EUHPC cluster.
    if [[ $(hostname --fqdn) =~ .*euhpc.* ]]; then
        my_module load gnu/tmux/2.3
    fi
fi
