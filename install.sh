#!/usr/bin/env bash
# Install the utility scripts for a new computer or VM.
# Parse the given arguments, and determine what to do from there.

# Figure out where we have placed the script.
dotfiles_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Put all arguments in a proper array for processing.
args=("$@")

# Setup the environment to create and the commands to run.
cmds=()
declare -A env

print-help()
{
    cat <<EOF
    -h         --help                Display this help text.

    -i         --install-packages    Enable installation of repository software
                                     packages.

    -e         --external-programs   Install all external utilities.

    -d         --dotfiles            Install all configuration dotfiles.

    -a         --autostart           Install autostart programs.

    -f         --fonts               Install extra fonts.

    -w         --work                Install the configuration used at work.

    -c         --clean               Clean up all the previously installed
                                     configuration.

    -p         --prompt OPT          Select the type of prompt to use. OPT
                                     may be one of: {heavy, light}.
                                     Default: heavy.

    -o DIR     --objects DIR         Set the directory to use for compiled
                                     objects and external tools.
                                     Default: "$HOME/git/installs" or the
                                     environment variable "objects_dir".

    -l DIR     --local-prefix DIR    Set the local prefix for binaries and man
                                     pages.
                                     Default: "$HOME/.local/" or the environment
                                     variable "local_prefix_dir".

    -tc NUM    --colors NUM          Force the use of 8, 16, 88 or 256 colors
                                     in the terminal windows. Any other values
                                     are invalid.

EOF
    exit 1
}

defaults()
{
    objects_dir=${objects_dir-"$HOME/git/installs"}
    local_prefix_dir=${local_prefix_dir-"$HOME/.local"}
    XDG_CONFIG_HOME=${XDG_CONFIG_HOME-"$HOME/.config"}
    XDG_DATA_HOME=${XDG_DATA_HOME-"${local_prefix_dir}/share"}
    env["dotfiles_dir"]="${dotfiles_dir}"
    env["objects_dir"]="${objects_dir}"
    env["local_prefix_dir"]="${local_prefix_dir}"
    env["PROMPT_COMMAND"]="${PROMPT_COMMAND}"
}

install-packages()
{
    echo "Installing software packages."
    . $dotfiles_dir/scripts/install_packages.sh
    return 0
}

install-external-programs()
{
    echo "Installing all external software packages."
    . $dotfiles_dir/scripts/install_external_scripts.sh
    . $dotfiles_dir/scripts/install_tmux.sh
    . $dotfiles_dir/scripts/install_universal_ctags.sh
    . $dotfiles_dir/scripts/install_global.sh
    . $dotfiles_dir/scripts/install_emacs.sh
}

dotfiles()
{
    echo "Installing dotfiles."
    . $dotfiles_dir/scripts/install_dotfiles.sh
    return 0
}

autostart()
{
    echo "Adding autostart files."
    . $dotfiles_dir/scripts/install_autostart.sh
    return 0
}

install-fonts()
{
    echo "Installing fonts."
    . $dotfiles_dir/scripts/install_fonts.sh
    return 0
}

work-config()
{
    echo "Installing (ARM) work configuration."
    . $dotfiles_dir/scripts/install_arm.sh
    return 0
}

clean()
{
    echo "Cleaning up installed configuration."
    . $dotfiles_dir/scripts/clean_dotfiles.sh
    return 0
}


# Set the default options
defaults

i=0
while [ $i -lt ${#args[@]} ]; do
    a=${args[$i]}
    if [ $a == "-h" -o $a == "--help" ]; then
	print-help
    elif [ $a == "-i" -o $a == "--install-packages" ]; then
	cmds+=(install-packages)
    elif [ $a == "-e" -o $a == "--external-programs" ]; then
	cmds+=(install-external-programs)
    elif [ $a == "-d" -o $a == "--dotfiles" ]; then
	cmds+=(dotfiles)
    elif [ $a == "-a" -o $a == "--autostart" ]; then
	cmds+=(autostart)
    elif [ $a == "-f" -o $a == "--fonts" ]; then
	cmds+=(install-fonts)
    elif [ $a == "-w" -o $a == "--work" ]; then
	cmds+=(work-config)
    elif [ $a == "-c" -o $a == "--clean" ]; then
	cmds+=(clean)
    elif [ $a == "-p" -o $a == "--prompt" ]; then
        : $(( i = i + 1 ))
        a=${args[$i]}
        if [ $a == "heavy" -o $a == "light" ]; then
            env["PROMPT_COMMAND"]=${a}_prompt
        else
            printf "Invalid prompt value.\n"
	    print-help
        fi
    elif [ $a == "-o" -o $a == "--objects" ]; then
	: $(( i = i + 1 ))
	a=$(readlink --canonicalize ${args[$i]})
	objects_dir="${a}"
	env["objects_dir"]="${a}"
    elif [ $a == "-l" -o $a == "--local-prefix" ]; then
	: $(( i = i + 1 ))
	a=$(readlink --canonicalize ${args[$i]})
	local_prefix_dir="${a}"
	env["local_prefix_dir"]="${a}"
    elif [ $a == "-tc" -o $a == "--colors" ]; then
	 : $(( i = i + 1 ))
	 a=${args[$i]}
	 if [ $a -eq 8  -o $a -eq 16 -o $a -eq 88 -o $a -eq 256 ]; then
	     force_colors="${a}"
	     env["force_colors"]="${a}"
	 else
	     printf "Invalid color value.\n"
	     print-help
	 fi
    else
	printf "Unknown argument or flag: %s\n" $a
	print-help
    fi

    : $(( i = i + 1 ))
done

# Run the actual commands with the arguments.
for c in ${cmds[@]}; do
    $(expr $c)
done
exit 0
