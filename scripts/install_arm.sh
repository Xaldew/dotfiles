#!/usr/bin/env bash
# This script adds some additional utilities used within ARM.

# Append the work configuration (below) to the .bashrc file.
# Note that doing this elsewhere will likely break things.
cat >> ~/.bashrc <<EOF

# Source common utilities.
source \$dotfiles_dir/scripts/get_distribution.sh

# Initialize tool to load modules.
source /arm/tools/setup/init/bash

#### Bash settings
shopt -s expand_aliases

# Source common module load script, located in the project folder:
source \$WORKSPACE_DIR/script/bash/module_load.sh 2> /dev/null

#### Setup some variables
export MODELSIM=\$WORKSPACE_DIR/modelsim.ini
export WORK=/work
export DESIGNKIT=/projects/mpd/designkit/
export LM_LICENSE_FILE=\$LM_LICENSE_FILE:7010@cam-lic3.cambridge.arm.com ;
export STYX3_AVESW_PATH="/work/mve/linux"
export ARGUS_IPADDR=10.44.10.244

#### Exports
export ARM_PROJECT_NR=PJ01433
export SCRATCH="/arm/scratch/\$USER"
export HW_UTILS_DIR="\$WORK/asic_util"
export WORKSPACE_DIR="\$WORK/mve6"
export MVE_MODEL_DIR="\$SCRATCH/git-mve6/model"
export GIT_HOME_DIR="\$HOME/git"

#### Axi-tester
export BUILDMAKEFLAGS="--silent -r"
export PLATFORM=i686-linux
export SVNROOT=http://lun-svn1.lund.arm.com/svn/mpd/video

#### Put some utils in PATH:
export PATH=\$HW_UTILS_DIR/thirdparty/jm/jm14.0/bin:\$PATH
export PATH=\$WORKSPACE_DIR/test/util/funtest:\$PATH
export PATH=\$WORKSPACE_DIR/test/system/out/util-linux64:\$PATH
export PATH=\$WORKSPACE_DIR/test/util/yuvtools:\$PATH
export PATH=\$WORKSPACE_DIR/bench/regression_cluster:\$PATH
export PATH=\$HW_UTILS_DIR/i686-linux:\$PATH


# Run 'module avail' for full list of available modules.
# Run 'module list' to list currently loaded modules.
module load eda
module load swdev
module load util
module load arm/rascdevkit/1.1.4
module load ccache/ccache/3.1.4
module load gnu/gdb/7.5          # No python scripting support.
module load gnu/valgrind/3.8.1
module load gnu/cmake/3.2.3
module load gnu/autoconf

module load scons/scons/2.3.0
module load swig/swig/2.0.0
module load apache/subversion/1.7.3
module load doxygen/doxygen/1.8.2
module load codesourcery/linuxeabi/arm-2011q1
module load smartbear/codecollab/8.1.8100

if [ "\`get_dist -i\`" = "ubuntu" ]; then
   # Running Ubuntu. Fix broken default packages.
   module load git/git/2.7.0
   module unload python/python
   module unload imagemagick/imagemagick
else
   # Running CentOS. Load CentOS specific packages.
   module load vim/vim/7.3
   module load git/git/v2.0.0
fi

#### Aliases
alias maketest=" (goto_repo && cd test/system/ && \
    make -j12 MVE_VERSION=V500_R0P1_00REL0 && \
    make virtex7 MVE_VERSION=V500_R0P1_00REL0 -j12)"
alias makerel="  (goto_repo && cd test/system/ && \
    RELEASE=1 make RELEASE=1 MVE_VERSION=V500_R0P1_00REL0 -j12 && \
    make virtex7 RELEASE=1 MVE_VERSION=V500_R0P1_00REL0 -j12)"
alias makedbg="  (goto_repo && cd test/system/ && \
    DEBUG=1 make DEBUG=1 MVE_VERSION=V500_R0P1_00REL0 -j12)"
alias makeclean="(goto_repo && cd test/system/ && make clean)"
alias makeutil=" (goto_repo && cd test/system/ && make util -j12)"

# bjobs/bwhat to monitor queue
alias bi="bsub -Is -P \$ARM_PROJECT_NR -R 'rhe6 && os64' bash"
alias bs='bsub -P PJ01433 -q normal -R "rhe6 && os64"'
alias bissh="ssh lun-login2.lund.arm.com"


# Preserve the PATH variable when entering (local) sudo.
alias sudo='sudo env PATH=\$PATH'

# Use local python install for Python 3.
alias python3='python3.4'


#### Functions
function runtest()
{
    (goto_repo && cd test/system/; ./runtest \$@)
}

## Goto the nearest the mve6 repo. If none is found, goto the default one
function goto_repo()
{
    pwd=\$(pwd)
    if [[ \$pwd == *mve6* ]]; then
	repo=\$(pwd | sed 's/mve6.*//')
	cd \$repo/mve6
    else
	cd \$WORKSPACE_DIR/mve6
    fi
}

function count_h264_frames()
{
    ffprobe -count_frames -show_frames -hide_banner "\$1" | \
        grep pict_type | sort | uniq -c
}

# Setup environment for building android/android-kernel stuff.
function android_env()
{
    location=\${1:-/ssd_data}
    export SSD=/ssd_data
    export DEMO=\${location}
    export ANDROID_BASE=\$DEMO/android
    export TOP=\$ANDROID_BASE
    export KDIR=\$DEMO/kernel
    export ROOTFS=\$DEMO/rootfs
    export ANDROID_PRODUCT_OUT=\$ANDROID_BASE/out/target/product/juno-eng
    export ANDROID_HOST_OUT=\$ANDROID_BASE/out/host/linux-x86/
    export ARCH=arm64
    export HW=1
    export CROSS_COMPILE=aarch64-linux-gnu-
    export STAY_OFF_MY_LAWN=1
    export USE_CCACHE=1
    export CCACHE_DIR=\$SSD/.ccache

    # Use system default instead of these broken modules.
    module unload sun/jdk/1.8.0_11
    module unload python/python
    if [ -d "\$ANDROID_BASE" ]; then
        source \$ANDROID_BASE/build/envsetup.sh
        pushd \$ANDROID_BASE > /dev/null
        lunch juno-eng

        if [ ! -d "\$CCACHE_DIR" ]; then
             prebuilts/misc/linux-x86/ccache/ccache -M 50G
        fi

        popd > /dev/null
    fi
    # Currently, the demobox resides on this IP.
    export ADBHOST=10.44.11.22
}

function strip_h264()
{
    in=\${1}
    ffmpeg -i "\$in" -vcodec copy -an -bsf:v h264_mp4toannexb "\${in%.*}.h264"
}

# Set the arm git config for ARM project repos.
# Otherwise, commits will not be pushable.
function set_arm_gitconfig()
{
    for arg in "\$@";
    do
	(cd \$arg && git config user.email "gustaf.waldemarson@arm.com")
    done
}

# Setup of TI2 (VIDEO) Environment Variables
export MPDTI_V2_USER=guswal01
export MPDTI_V2_PIC_SERVER=lun-mpdti2.lund.arm.com:55300
export MPDTI_V2_PROJECT=PJ00640

# Add some paths for work based utilities.
export PATH=\$local_prefix_dir/bin:\$PATH
export PATH=/work/local/bin:\$PATH

# Additional paths for tex-live (2015).
export PATH=\$PATH:/usr/local/texlive/2015/bin/x86_64-linux
export INFOPATH=\$INFOPATH:/usr/local/texlive/2015/texmf-dist/doc/info
export MANPATH=\$MANPATH:/usr/local/texlive/2015/texmf-dist/doc/man


# Sets a more 'modern' termcap file for use with emacs, otherwise colors will be
# messed up. Only needed at work.
export TERMCAP=\$HOME/.termcap

# Override the prompt override.
source $dotfiles_dir/scripts/prompt.sh
export PROMPT_COMMAND=light_prompt

# Reset LC_ALL to unset.
export LC_ALL=
export LANG=en_US.utf8
export LANGUAGE=en_US.utf8

# Undo GNU global tags configuration - requires pygments.
unset GTAGSCONF
unset GTAGSLABEL
EOF
