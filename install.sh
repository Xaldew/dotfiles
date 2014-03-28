#!/usr/bin/env bash
# Install the utility scripts for a new computer or VM.
# Parse the given arguments, and determine what to do from there.
# @TODO: Calculate the length of the flags and descriptions.
# @TODO: Longer descriptions should wrap appropriately.

# Figure out where we have placed the script.
DOTFILES_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Put all arguments in a proper array for processing.
arguments=("$@")

# Default options.
ndefaults=1
declare -a short_defaults=(-d)
declare -a long_defaults=(
    --dotfiles
)

# Linked options.
nlinked_options=5
declare -a short_options=(-h -i -d -a -f)
declare -a long_options=(
    --help
    --install-packages
    --dotfiles
    --autostart
    --install-fonts
)
declare -a descriptions=(
    "Display this help screen."
    "Enable installation of software packages."
    "Enable installation of dotfiles. (Enabled by default.)"
    "Enable installation of autostart files."
    "Enable installation of font files."
)

# linked options lengths.
short_length=5
long_length=25
desc_length=50

function add-default-options()
{
    for ((i=0; i < $ndefaults; ++i));
    do
	found=false
	for arg in ${arguments[@]};
	do
	    if [ $arg == ${short_defaults[i]} -o \
		    $arg == ${long_defaults[i]} ]; then
		found=true
	    fi
	done
	if [ $found == false ]; then
	    arguments+=(${long_defaults[i]})
	fi
    done
}

function test-options()
{
    arg=$1
    for ((i=0; i < $nlinked_options; ++i));
    do
	if [ $arg == ${short_options[i]} -o $arg == ${long_options[i]} ]; then
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

add-default-options

for arg in ${arguments[@]};
do
    test-options $arg
done

echo "Installation finished successfully."
