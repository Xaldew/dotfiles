#!/usr/bin/env bash
# Install the utility scripts for a new computer or VM.
# Parse the given arguments, and determine what to do from there.
# @TODO: Calculate the length of the flags and descriptions.
# @TODO: Longer descriptions should wrap appropriately.

# Figure out where we have placed the script.
DOTFILES_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Put all arguments in a proper array for processing.
arguments=("$@")

# Linked options.
nlinked_options=7
declare -a short_options=(-h -i -d -a -f -w -c)
declare -a long_options=(
    --help
    --install-packages
    --dotfiles
    --autostart
    --install-fonts
    --work-config
    --clean
)
declare -a descriptions=(
    "Display this help screen."
    "Enable installation of software packages."
    "Enable installation of dotfiles."
    "Enable installation of autostart files."
    "Enable installation of font files."
    "Enable installation of work configuration."
    "Clean up all the previously installed configurations."
)

# linked options lengths.
short_length=5
long_length=25
desc_length=50

function convert-options()
{
    # Convert each argument to their long format equivalent.
    for ((i=0; i < ${#arguments[@]}; ++i));
    do
	arg=${arguments[i]}
	for ((j=0; j < ${#short_options[@]}; ++j));
	do
    	    if [ "$arg" == "${short_options[j]}" ]; then
    		arguments[i]=${long_options[j]}
    	    fi
	done
    done

    # Verify that all arguments has been converted and exists.
    for ((i=0; i < ${#arguments[@]}; ++i));
    do
	found=0
	arg=${arguments[i]}
	for ((j=0; j < ${#long_options}; ++j));
	do
    	    if [ "$arg" == "${long_options[j]}" ]; then
    		arguments[i]=${long_options[j]}
       		found=1
    	    fi
	done

	# Exit if argument wasn't found. not found.
	if [ $found -eq 0 ]; then
	    echo "Unrecognized argument: $arg"
	    help
	fi

    done

    return 0
}

function test-options()
{
    arg=$1
    for ((i=0; i < $nlinked_options; ++i));
    do
	if [ $arg == ${long_options[i]} ]; then
	    $(expr substr ${long_options[i]} 3 ${#long_options[i]})
	    return 0
	fi
    done
    printf "Unrecognized argument: %s.\n" ${arg}
    exit 1
}

function help()
{
    for ((i=0; i < $nlinked_options; ++i));
    do
	printf "%-*s %-*s %-*s\n" \
	    ${short_length} ${short_options[i]} \
	    ${long_length} ${long_options[i]}  \
	    ${desc_length} "${descriptions[i]}"
    done
    exit 0
}

function install-packages()
{
    echo "Installing software packages."
    source $DOTFILES_DIR/scripts/install_packages.sh
    return 0
}

function dotfiles()
{
    echo "Installing dotfiles."
    source $DOTFILES_DIR/scripts/install_dotfiles.sh
    return 0
}

function autostart()
{
    echo "Adding autostart files."
    source $DOTFILES_DIR/scripts/install_autostart.sh
    return 0
}

function install-fonts()
{
    echo "Installing fonts."
    source $DOTFILES_DIR/scripts/install_fonts.sh
    return 0
}

function work-config()
{
    echo "Installing (ARM) work configuration."
    source $DOTFILES_DIR/scripts/install_arm.sh
    return 0
}

function clean()
{
    echo "Cleaning up installed configuration."
    source $DOTFILES_DIR/scripts/clean_dotfiles.sh
    return 0
}

# Convert the short options to the equivalent long ones.
convert-options

# Loop over the arguments and execute the appropriate function.
for arg in ${arguments[@]};
do
    $(expr substr $arg 3 ${#arg})
done

echo "Installation finished successfully."
exit 0
