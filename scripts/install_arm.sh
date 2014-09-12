#!/usr/bin/env bash
# This script adds some additional utilities used within ARM.




#read -d '' arm_conf <<"EOF"
# Initialize tool to load modules.
source /arm/tools/setup/init/bash

#### Bash settings
shopt -s expand_aliases

#### Exports
export TERM=xterm-256color
export LC_ALL=en_US.utf8
export LANG=en_US.utf8
export WORKSPACE_DIR="/home/$USER/git"

#source common module load script, located in the project folder:
source $WORKSPACE_DIR/mve6/script/bash/module_load.sh

#### Setup some variables
export MODELSIM=$WORKSPACE_DIR/mve6/modelsim.ini
export MTI=${MODELTECH_HOME}
export WORK=/work/${USER}
export DESIGNKIT=/projects/mpd/designkit/
export LM_LICENSE_FILE=$LM_LICENSE_FILE:7010@cam-lic3.cambridge.arm.com ;
export STYX3_AVESW_PATH="/work/linux"
export ARGUS_IPADDR=10.44.10.244

#### Axi-tester
export SPYGLASS_POLICIES=$WORKSPACE_DIR/mve6/spyglass_policies
export BUILDMAKEFLAGS="--silent -r"
export PLATFORM=i686-linux
export SVNROOT=http://lun-svn1.lund.arm.com/svn/mpd/video

#### Put some utils in PATH:
export PATH=$WORKSPACE_DIR/asic/util/i686-linux:$PATH
export PATH=$WORKSPACE_DIR/mve6/util/jm/jm14.0/bin:$PATH
export PATH=$WORKSPACE_DIR/mve6/test/system/out/util-linux64:$PATH
export PATH=$WORKSPACE_DIR/mve6/test/util/yuvtools:$PATH

#EOF
