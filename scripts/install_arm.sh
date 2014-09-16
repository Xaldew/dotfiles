#!/usr/bin/env bash
# This script adds some additional utilities used within ARM.

# Append the work configuration (below) to the .bashrc file.
# Note that doing this elsewhere will likely break things.
cat >> ~/.bashrc <<EOF

# Initialize tool to load modules.
source /arm/tools/setup/init/bash

#### Bash settings
shopt -s expand_aliases

#### Exports
export LC_ALL=en_US.utf8
export LANG=en_US.utf8
export SCRATCH="/scratch/\$USER"
export WORKSPACE_DIR="/home/\$USER/git"

#source common module load script, located in the project folder:
source \$WORKSPACE_DIR/mve6/script/bash/module_load.sh 2> /dev/null

#### Setup some variables
export MODELSIM=\$WORKSPACE_DIR/mve6/modelsim.ini
export MTI=\${MODELTECH_HOME}
export WORK=/work/\${USER}
export DESIGNKIT=/projects/mpd/designkit/
export LM_LICENSE_FILE=\$LM_LICENSE_FILE:7010@cam-lic3.cambridge.arm.com ;
export STYX3_AVESW_PATH="/work/linux"
export ARGUS_IPADDR=10.44.10.244

#### Axi-tester
export SPYGLASS_POLICIES=\$WORKSPACE_DIR/mve6/spyglass_policies
export BUILDMAKEFLAGS="--silent -r"
export PLATFORM=i686-linux
export SVNROOT=http://lun-svn1.lund.arm.com/svn/mpd/video

#### Put some utils in PATH:
export PATH=\$WORKSPACE_DIR/asic/util/i686-linux:\$PATH
export PATH=\$WORKSPACE_DIR/mve6/util/jm/jm14.0/bin:\$PATH
export PATH=\$WORKSPACE_DIR/mve6/test/system/out/util-linux64:\$PATH
export PATH=\$WORKSPACE_DIR/mve6/test/util/yuvtools:\$PATH


#### Load a few modules from /arm/tools/modulefiles
# Run 'module avail' for full list
## These do not work on Ubuntu 12.04.
# module load coverity/static-analysis/5.2.1
# module load gnu/autoconf/2.68
# module load gnu/automake/1.11.3
# module load gnu/make/3.81
# module load gnu/m4/1.4.12
# module load lcov/lcov/1.9
# module load meld/meld/1.5.3
# module load python/numpy2.7/1.6.2
# module load python/matplotlib_py2.7/1.2.1
# module load python/scipy_py2.7/0.12.0

module load eda
module load swdev
module load util
module load vim/vim/7.3
module load arm/rascdevkit/1.1.4
module load ccache/ccache/3.1.4
module load gnu/emacs/24.2
module load gnu/gdb/7.5
module load gnu/valgrind/3.8.1
module load gnu/cmake/2.8.9
module load python/python/2.7.1
module load scons/scons/2.3.0
module load swig/swig/2.0.0
#module load git/git/1.7.9.2 # Issues with android repo client.
#module load git/git/v2.0.0 # Missing git-svn.
module load apache/subversion/1.7.3
module load doxygen/doxygen/1.8.2
module load codesourcery/linuxeabi/arm-2011q1
module load smartbear/codecollab/8.1.8100

#### Aliases
alias maketest=" (goto_repo && cd test/system/ && make -j12 MVE_VERSION=V500_R0P1_00REL0 && make virtex7 MVE_VERSION=V500_R0P1_00REL0 -j12)"
alias makerel="  (goto_repo && cd test/system/ && RELEASE=1 make RELEASE=1 MVE_VERSION=V500_R0P1_00REL0 -j12 && make virtex7 RELEASE=1 MVE_VERSION=V500_R0P1_00REL0 -j12)"
alias makedbg="  (goto_repo && cd test/system/ && DEBUG=1 make DEBUG=1 MVE_VERSION=V500_R0P1_00REL0 -j12)"
alias makeclean="(goto_repo && cd test/system/ && make clean)"
alias makeutil=" (goto_repo && cd test/system/ && make util -j12)"
alias bi="bsub -Is -P PJ00640 -R 'rhe6 && os64'" # bjobs/bwhat to monitor queue
alias lg="log --graph --pretty=format:'%Cred%h%Creset - %C(bold blue)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)' --abbrev-commit"


#### Functions
runtest(){
  (goto_repo && cd test/system/; ./runtest \$@)
}

## Goto the nearest the mve6 repo. If none is found, goto the default one
goto_repo(){
  pwd=\$(pwd)
  if [[ \$pwd == *mve6* ]]
  then
  repo=\$(pwd | sed 's/mve6.*//')
    cd \$repo/mve6
  else
    cd \$WORKSPACE_DIR/mve6
  fi
}

# Setup of TI2 (VIDEO) Environment Variables
export MPDTI_V2_USER=guswal01
export MPDTI_V2_PIC_SERVER=lun-mpdti2.lund.arm.com:55300
export MPDTI_V2_PROJECT=PJ00640
export PATH=\$PATH:~/.local/bin

# Sets a more 'modern' termcap file for use with emacs, otherwise colors will be
# messed up. Only needed at work.
export TERMCAP=\$HOME/.termcap

EOF
