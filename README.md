# Xaldew's Dotfiles
This is repository containing my personal configuration for various programs. It
is only intended to be portable to a limited number of platforms but still be
easily installable on a new computer.

## Unix Installation
The install-script assumes use of a fairly modern version of Bash. To be
specific, it needs Bash version 4 for the associative arrays.

The scripts execute commands based on the flags it is given. Also, several
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

    -r DIR     --ram-disk DIR        Select a directory to set up as a RAMDISK.
                                     (Not yet implemented.)

## Windows Installation
The "windows" folder contains PowerShell scripts that will install a subset of
the configuration that is usable on both Windows and Unix.

Configuration specific to windows is located here as well.

Note that to actually run many of these PowerShell scripts, the execution policy
must be at least *RemoteSigned*, i.e., scripts downloaded from somewhere has to
be properly signed. You can set that policy with the following PowerShell command:

    Set-ExecutionPolicy Remotesigned

To install packages using any of the package installation script listed below,
you must additionally use an *Administrative* PowerShell prompt.


### Installing Dotfiles

To install the dotfiles on any PowerShell v2.0 or higher platform, run the
PowerShell script called: `install_dotfiles.ps1`. This will install the
Bash, Emacs, AutoHotkey and autostart settings.


### PowerShell Downloader

This is the most basic installation and downloading script. It is simply called
`install_portable.ps1` since it is technically the most portable script. This
script was written mostly as a practice in writing PowerShell code. It simply
lists URLs for the packages, downloads and installs them. You still have to
click through all the installers however.


### Chocolatey Installation

The `install_chocolatey.ps1` script installs the Chocolatey program if it is
missing, and then uses Chocolatey infrastructure to install the rest of the
packages. Note however that it is not always the most recent version that is
present in the Chocolatey repositories.


### Windows 10 Packet Manager

The `install_packages.ps1` is from now on the primary Windows package
installation script. Windows 10 now bundles the package manager *OneGet* with
the OS, making it fairly simple to configure it to receive packages from the
Chocolatey infrastructure.
