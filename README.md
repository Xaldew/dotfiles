# Xaldew's Dotfiles
This is repository containing my personal configuration for various programs. It
is only intended to be portable to a limited number of platforms but still be
easily installable on a new computer.

## Unix Installation
The install script assumes use of a fairly modern version of Bash. To be
specific, it needs Bash version 4 for the associative arrays.

The scripts executes commands based on the flags it is given. Also, several
options are available to move install locations and setting various options.

### Flags
Currently, the following flags exists:

    -h         --help                Display this help text.

    -i         --install-packages    Enable installation of repository software
                                     packages.

    -e         --external-programs   Install all external utilities.

    -d         --dotfiles            Install all configuration dotfiles."

    -a         --autostart           Install autostart programs.

    -f         --fonts               Install extra fonts.

    -w         --work                Install the configuration used at work.

    -c         --clean               Clean up all the previously installed
                                     configuration.

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

    -r DIR     --ram-disk DIR        Select a directory to setup as a RAMDISK.
                                     (Not yet implemented.)

## Windows Installation
The "windows" folder contains PowerShell scripts that will install a subset of
the configuration that is usable on both Windows and Unix.

Configuration specific to windows is located here as well.


### Installing Dotfiles


### PowerShell Downloader

This is the most basic installation script. It was written mostly as a practice
in writing PowerShell code. It simply lists URLs for the packages, downloads and
installs them.


### Chocolatey Installation


### Windows 10 Packet Manager
