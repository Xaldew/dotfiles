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
source $WORKSPACE_DIR/mve6/script/bash/module_load.sh >2 /dev/null

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


#### Load a few modules from /arm/tools/modulefiles
# Run 'module avail' for full list
module load arm/rascdevkit/1.1.4
module load ccache/ccache/3.1.4
module load coverity/static-analysis/5.2.1
module load gnu/autoconf/2.68
module load gnu/automake/1.11.3
#module load gnu/binutils/2.23.1
module load gnu/cmake/2.8.9
#module load gnu/gcc/4.1.2
module load gnu/gdb/7.5
module load gnu/m4/1.4.12
module load gnu/make/3.81
module load gnu/valgrind/3.8.1
module load git/git/1.7.9.2
module load lcov/lcov/1.9
module load meld/meld/1.5.3
module load smartbear/codecollab/8.1.8100
module load vim/vim/7.3
module load eda
module load swdev
module load util
module load apache/subversion/1.7.3
module load gnu/emacs/24.2
#module load git/git/v2.0.0
module load doxygen/doxygen/1.8.2
module load gnu/cmake/2.8.9
module load python/python/2.7.1
module load scons/scons/2.3.0
module load swig/swig/2.0.0
module load python/numpy2.7/1.6.2
module load python/matplotlib_py2.7/1.2.1
module load python/scipy_py2.7/0.12.0
module load codesourcery/linuxeabi/arm-2011q1


#### Aliases
alias maketest=" (goto_repo && cd test/system/ && make -j12 MVE_VERSION=V500_R0P1_00REL0 && make virtex7 MVE_VERSION=V500_R0P1_00REL0 -j12)"
alias makerel="  (goto_repo && cd test/system/ && RELEASE=1 make RELEASE=1 MVE_VERSION=V500_R0P1_00REL0 -j12 && make virtex7 RELEASE=1 MVE_VERSION=V500_R0P1_00REL0 -j12)"
alias makedbg="  (goto_repo && cd test/system/ && DEBUG=1 make DEBUG=1 MVE_VERSION=V500_R0P1_00REL0 -j12)"
alias makeclean="(goto_repo && cd test/system/ && make clean)"
alias makeutil=" (goto_repo && cd test/system/ && make util -j12)"
alias bi="bsub -Is -P PJ00640 -R 'rhe6 && os64'" # bjobs/bwhat to monitor queue
alias ll='ls -ll'
alias lg="log --graph --pretty=format:'%Cred%h%Creset - %C(bold blue)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)' --abbrev-commit"


#### Functions
runtest(){
  (goto_repo && cd test/system/; ./runtest $@)
}

## Goto the nearest the mve6 repo. If none is found, goto the default one
goto_repo(){
  pwd=$(pwd)
  if [[ $pwd == *mve6* ]]
  then
  repo=$(pwd | sed 's/mve6.*//')
    cd $repo/mve6
  else
    cd $WORKSPACE_DIR/mve6
  fi
}

#source common module load script, located in the project folder:
#source /scratch/guswal01/svn/mve6/script/bash/module_load.sh

# Load handpicked modules.
# Setup of TI2 (VIDEO) Environment Variables
export MPDTI_V2_USER=guswal01
export MPDTI_V2_PIC_SERVER=lun-mpdti2.lund.arm.com:55300
export MPDTI_V2_PROJECT=PJ00640

# Variable for quick-return to system_test
#export my_project=/scratch/guswal01/svn/system_test/

# Add TI2 mpdtiv2_cli command tool to the path:
export PATH=$PATH:~/.local/bin
PROMPT_DIRTRIM=2

# Sets a more 'modern' termcap file for use with emacs.
export TERMCAP=$HOME/.termcap

# Add default grep options
export GREP_OPTIONS='--exclude-dir=.{svn,git}'

alias open="xdg-open"

alias cmake="cmake -D CMAKE_C_COMPILER=gcc -D CMAKE_CXX_COMPILER=g++"

# License server for arm DS-5.
export ARMLMD_LICENSE_FILE=7010@cam-lic03.cambridge.arm.com

###########
# Config from home:
###########
# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
    fi
fi

# Add 256 colour support for the xterm emulators.
export TERM=xterm-256color

# Set the terminal to ignore filename completion for more suffixes.
export FIGNORE=$FIGNORE:svn:.o:.~:\#*\#:.pyc:.pyo

# Add a variable for quick scratch access.
export SCRATCH="/scratch/guswal01"

# Setup emacs as the default editor.
if [[ ! $EDITOR =~ .*emacs.* ]]; then
    export EDITOR="emacs"
fi


## Simplifies using the mve_decode tool
## Copies source material and outputs to local directory to speed up testing
mve(){
  # Print help
  if [ $# -lt 2 ]; then
    echo -e "\n\tusage: mve [codec] [input video] [decoder options]\n\n" \
            "\t--cmd      print the full codec command instead of running it\n" \
            "\t--gdb-emul run the emulator command in gdb\n" \
            "\t--gdb-fw   run the firmware command in gdb\n" \
            "\t--less     pipe output to less (not gdb compatible)\n" \
            "\t--mbinfo   give the codec the --mbinfo flag and output to .mbinfo file\n" \
            "\t--valgrind run the emulator in valgrind\n"
    return 1
  fi
  # Setup dir structure
  mkdir -p /work/$USER/tmp;
  output="test.yuv"
  # Save arguments, since the shift in the while loop will consume them
  codec="$1";
  input="$2";
  input_filename="$(basename $2)"
  mve_app="mve_decode"
  # Initialize flags
  print_command=false
  gdb_emul=false
  gdb_fw=false
  valgrind=false
  # Remove codec and input path arguments
  shift; shift
  # Create an empty array of arguments to be passed onto the emulator
  args=()

  # If codec is an encoder, use encoder binary instead
  if [[ $codec == *enc* ]]; then
    mve_app="mve_encode"
  fi

  # Handle arguments
  for argument; do
    # Look for --cmb argument, if found output command instead of executing it
    if [ "$argument" = "--cmd" ]; then print_command=true; input_filename=$input
    # Look for --gdb-emul argument, if found run the emulator in gdb
    elif [ "$argument" = "--gdb-emul" ]; then gdb_emul=true;
    # Look for --gdb-fw argument, if found run the fw in gdb
    elif [ "$argument" = "--gdb-fw" ]; then gdb_fw=true;
    # Look for --mbinfo argument, if found append ".mbinfo" to output filename
    elif [ "$argument" = "--mbinfo" ]; then output="$output.mbinfo"; args+=($argument);
    # Look for --valgrind", run the emulator in valgrind
    elif [ "$argument" = "--valgrind" ]; then  valgrind=true;
    else args+=($argument);
    fi
  done

  # Build executable command (needs to be declared after the args array is done)
  command="$WORKSPACE_DIR/mve6/test/system/out/test-linux32/$mve_app --fwbin $WORKSPACE_DIR/mve6/test/system/out/codec-$codec/codec.fwbin --name $codec -i $input_filename -o $output ${args[@]}"

  (
    cd /work/$USER/tmp;
    rsync -aq $input .;

    if $print_command; then
      input_filename=input
      echo "$command"
    elif $gdb_emul; then
      echo -e "set logging on\nset logging overwrite on\nset logging file $output.less" > gdb.commands
      chmod +x gdb.commands
      eval "gdb -x gdb.commands --args $command"
    elif $gdb_fw; then
      echo -e "# Debug using GDB?\ngdb-enable=yes\n# Port number for GDB communication.\ngdb-port=$(id -u)" > profile.conf
      # Backup environment variable
      env_backup=$DESKTOP_STARTUP_ID
      # Disable focus stealing in window manager
      export DESKTOP_STARTUP_ID=_TIME0
      gnome-terminal --title="emul" -x sh -c "$command --config profile.conf 2>&1 | tee $output.less" &
      # Re-enable focus stealing
      export DESKTOP_STARTUP_ID=$env_backup
      echo -e "target remote localhost:$(id -u)" > gdb.commands
      chmod +x gdb.commands
      # Give the emulator some time to start (or it won't accept the gdb connection)
      sleep 0.2
      rasc-elf-gdb "$WORKSPACE_DIR/mve6/test/system/out/codec-$codec/codec.elf" -x gdb.commands
    elif $valgrind; then
      valgrind --track-origins=yes $command
    else
      echo -e "codec\t\"$codec\""
      echo -e "input\t\"$input_filename\""
      echo -e "output\t\"$output\""
      echo -e "args\t\"${args[@]}\""
      eval "$command 2>&1 | tee $output.less"
    fi
  )
}
