#!/usr/bin/env bash
# This script adds some additional utilities used within ARM.

# Append the work configuration (below) to the .bashrc file. Note that doing
# this elsewhere will likely break things.
cat >> ~/.bashrc <<EOF
source ${dotfiles_dir}/scripts/arm_activate.sh
EOF

# Append the work (t)csh configuration to the cluster .cshrc file.
cat >> ~/.cshrc <<EOF
source /arm/tools/setup/init/tcsh
module load arm/cluster
EOF
