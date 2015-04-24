# Xaldew's Dotfiles
This is repository containing my personal configuration for various programs. It
is only intended to be portable to a limited number of platforms but still be
easily installable on a new computer.

## Installation.
The install script assumes use of a fairly modern Bash. It will likely be
updated at some point, but for now this is how you can use it.

The scripts executes commands based on the flags it is given. Currently, the
following flags exist:

* -h, --help: Show the available flags.
* -i, --install-packages: Install a listed set of Ubuntu/Fedora packages. Has no
effect on other systems.
* -d, --dotfiles: Install the program configuration dotfiles. Can download
additional utility scripts.
* -a, --autostart: Install autostart scripts. These are executed upon logging in
through the window system login manager.
* -f, --install-fonts: Install the source-code-pro font (and possibly others
later on).
* -w, --work-config: Inject additional utilities and configuration into .bashrc
to make the configuration work at work.
* -c, --clean: Remove all configuration dotfiles.
