#!/usr/bin/env sh
# Common utilities, aliases and functions usable by all POSIX shells.

source_if()
{
    # Source the given files if they exist.
    for f in "$@"; do
	if [ -e "$f" ]; then
	    . $f
	else
	    return
	fi
    done
}


command_exists()
{
    command -v "$1" >/dev/null 2>&1
}


open()
{
    # Open each of the given files with the prefered application.
    for f in "$@"; do
	xdg-open $f
    done
}


my-shell()
{
    # Get your current shell
    ps | grep `echo $$` | awk '{ print $4 }'
}


swap-caps()
{
    # Swap between a Vim friendly Caps == ESC and Emacs Caps == ctrl layout.
    # In practice, this swaps the X keyboard options: ctrl:nocaps and
    # caps:escape.
    current=`setxkbmap -query | awk '$1 ~ /^options/ { print $2 }'`
    if [ "$current" = "ctrl:nocaps" ]; then
        setxkbmap -option ''
        setxkbmap -option 'caps:escape' \
                  -option 'grp:sclk_toggle' \
                  -option 'grp:rctrl_rshift_toggle' \
                  -option 'grp_led:scroll' \
                  -layout se,us
    elif [ "$current" = "caps:escape" ]; then
        setxkbmap -option ''
        setxkbmap -option 'ctrl:nocaps' \
                  -option 'grp:sclk_toggle' \
                  -option 'grp:rctrl_rshift_toggle' \
                  -option 'grp_led:scroll' \
                  -layout se,us
    else
        printf "WARN: Unknown XKB option: %s\n" $current
    fi
}


mktmpfs()
{
    # Create a temporary file system in the given folder.
    path=${1-tmpfs}
    size=${2-512m}
    if [ -d "$path" ]; then
        if [ "`ls -A $path`" ]; then
            printf "Error: Non-empty directory: %s.\n" $path
        else
            sudo mount --types tmpfs --options size=$size,mode=777 $path
        fi
    else
        mkdir --parents $path
        sudo mount --types tmpfs --options size=$size,mode=777 $path
    fi
}


pdf-merge()
{
    # Merge the listed files into a single pdf called merged.pdf.
    gs \
	-dBATCH \
	-dNOPAUSE \
	-dQUIET \
	-sDEVICE=pdfwrite \
	-sOutputFile=merged.pdf \
	"$@"
}


pdf-extract()
{
    # Function to extract a range of pdf-pages using ghostscript.
    # This function uses 3 arguments:
    #    $1 is the input file
    #    $2 is the first page of the range to extract
    #    $3 is the last page of the range to extract
    #    The output file will be named "inputfile_pXX-pYY.pdf"
    input=${1}
    start=${2}
    end=${3}
    shift 3
    gs  "$@" \
	-sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER \
	-dFirstPage=${start} \
	-dLastPage=${end} \
	-sOutputFile=${input%.pdf}_p${2}-p${3}.pdf \
	${input}
}


pdf-gray()
{
    # Function to convert a PDF to another grayscale PDF.
    # this function uses 3 arguments:
    #    $1 is the input pdf file.
    #    The output file will be named "'inputfile'_grayscale.pdf"
    input=${1}
    output=$(basename ${1} .pdf)_grayscale.pdf
    shift 1
    gs  "$@" \
	-sOutputFile=${output} \
	-sDEVICE=pdfwrite \
	-sColorConversionStrategy=Gray \
	-dProcessColorModel=/DeviceGray \
	-dCompatibilityLevel=1.4 \
	-dNOPAUSE \
	-dBATCH \
	${input}
}


extract()
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)    tar xjf $1        ;;
            *.tar.gz)     tar xzf $1        ;;
            *.tar.xz)     tar xf $1         ;;
            *.bz2)        bunzip2 $1        ;;
            *.rar)        unrar x $1        ;;
            *.gz)         gunzip $1         ;;
            *.tar)        tar xf $1         ;;
            *.tbz2)       tar xjf $1        ;;
            *.tgz)        tar xzf $1        ;;
            *.zip)        unzip $1          ;;
            *.Z)          uncompress $1     ;;
            *)            printf "'$1' cannot be extracted via extract().\n" ;;
        esac
    else
        printf "'$1' is not a valid file.\n"
    fi
}


dls ()
{
    # List directories.
    dirs=`ls -l --color | grep "^d" | awk '{ print $9 }' | tr -d "/"`
    printf "%s\n" $dirs
}


dgrep()
{
    # A recursive, case-insensitive grep that excludes binary files.
    grep -iR "$@" * | grep -v "Binary"
}


dfgrep()
{
    # A recursive, case-insensitive grep that excludes binary files
    # and returns only unique filenames.
    grep -iR "$@" * | grep -v "Binary" | \
        sed 's/:/ /g' | awk '{ print $1 }' | sort | uniq
}


psgrep()
{
    # Grep for processes.
    if [ ! -z "$1" ] ; then
        printf "Grepping for processes matching '$1'...\n"
        ps aux | grep "$1" | grep -v grep
    else
        printf "WARN: Need name to grep for.\n"
    fi
}


unpatch()
{
    # Undo previous patching attempts.
    find . -name "*.orig" -o -name "*.rej"  -type f -exec rm {} \;
    find . -name "b" -type d -exec rm -rf {} \;
}


# Create a simple tree implementation if not already available.
if ! command_exists tree; then
    tree()
    {
	find "$@" -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'
    }
fi


rand_int()
{
    # Output a random signed integer.
    # Optional argument is the number of bytes to use for the number.
    size=${1-4}
    od --address-radix=n --read-bytes=${size} --format=d${size} /dev/urandom | \
        sed -e 's/^[ \t]*//'
}


rand_uint()
{
    # Output a random signed integer.
    # Optional argument is the number of bytes to use for the number.
    size=${1-4}
    od --address-radix=n --read-bytes=${size} --format=u${size} /dev/urandom | \
        sed -e 's/^[ \t]*//'
}


find_dropbox()
{
    # Function to find the dropbox folder.
    if [ -d "$HOME/Dropbox" ]; then
	printf "$HOME/Dropbox\n"
        return 0
    elif [ -f "$HOME/.dropbox/host.db" ]; then
        dir=`tail -n 1 $HOME/.dropbox/host.db 2> /dev/null | base64 -d`
        if [ -d "$dir" ]; then
	    printf "%s\n" $dir
            return 0
        else
            return 1
        fi
    else
        return 1
    fi
}


git-dl()
{
    # Download a single file from a git repository.
    # In most cases it may be more suitable to use wget or curl instead.
    #    $1: The URL to the desired repository.
    #    $2: The file we would like to extract.
    url="$1"
    file="$2"
    base_file=`basename ${file}`
    repo=`basename "${url}" ".git"`
    dir=`dirname $file`

    # Attempt to perform archival extraction from the remote.
    ret=0
    git archive --remote=$url HEAD:$dir $file 2> /dev/null | tar -x 2> /dev/null

    # Archiving retrieval failed.
    # Perform a shallow clone in /tmp and extract from there.
    if [ $? -ne 0 ]; then
	tmp_dir=`mktemp --directory`
	cwd=`pwd`
	cd $tmp_dir
	git clone --depth 1 $url > /dev/null 2> /dev/null && \
	    cd $repo >/dev/null &&
            git show HEAD:$file > $cwd/$base_file 2> /dev/null
	ret=$?
	cd $cwd
	rm -rf $tmp_dir
    fi
    return $ret
}


grep-find()
{
    # Grep for things from files found by find.
    find . -type f -exec grep --color -nH -e $1 {} +
}


## Emacs and emacsclient aliases and utility functions.

# Terminal Emacsclient.
nx-emacs-client()
{
    # Start the Emacs client toward the default Emacs daemon socket.
    # Append a new socket name to the arguments to use a different one.
    emacsclient --tty --socket-name=default --alternate-editor="" "$@"
}
alias em="nx-emacs-client"
alias ema="nx-emacs-client"
alias emac="nx-emacs-client"
alias emas="nx-emacs-client"
alias emasc="nx-emacs-client"
alias emascs="nx-emacs-client"
alias eam="nx-emacs-client"

# Start new non-client nx-Emacs instances.
alias nem="emacs --no-window-system"
alias nema="emacs --no-window-system"
alias neam="emacs --no-window-system"
alias nemac="emacs --no-window-system"
alias nemas="emacs --no-window-system"
alias nemacs="emacs --no-window-system"
alias nemasc="emacs --no-window-system"


# Graphical Emacsclient.
graphical-emacs-client()
{
    # Start the Emacs client toward the default Emacs daemon socket.
    # Append a new socket name to the arguments to use a different one.
    emacsclient --create-frame --socket-name=default --alternate-editor="" "$@" &
}
alias ge="graphical-emacs-client"
alias gem="graphical-emacs-client"
alias gema="graphical-emacs-client"
alias gemac="graphical-emacs-client"
alias gemacs="graphical-emacs-client"


# Start new non-client graphical-Emacs instances.
new-graphical-emacs()
{
    \emacs --maximized --no-desktop "$@" &
}
alias ngem="new-graphical-emacs"
alias ngema="new-graphical-emacs"
alias ngeam="new-graphical-emacs"
alias ngemac="new-graphical-emacs"
alias ngemas="new-graphical-emacs"
alias ngemacs="new-graphical-emacs"
alias ngemasc="new-graphical-emacs"


# Pipe stdout to temporary file and open that in an Emacs buffer.
nx-emacspipe()
{
    tmp=`mktemp` && cat > $tmp && nx-emacs-client $tmp ; rm $tmp
}
alias ep="nx-emacspipe"
alias emp="nx-emacspipe"
graphical-emacspipe()
{
    tmp=`mktemp` && cat > $tmp && graphical-emacs-client $tmp ; rm $tmp
}
alias gep="graphical-emacspipe"
alias gemp="graphical-emacspipe"


# Start a new Emacs daemon.
emacs-daemon()
{
    server=${1-default}
    shift
    \emacs --daemon=$server "$@"
}
alias emd=emacs-daemon
alias emad=emacs-daemon
alias emacd=emacs-daemon
alias emacsd=emacs-daemon


## Miscellaneous aliases.

# Enable color support of ls and also add handy aliases.
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || \
	    eval "$(dircolors -b)"
    # Add default grep options
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto --exclude-dir={\.svn,\.git,\.repo}'
    alias fgrep='fgrep --color=auto --exclude-dir={\.svn,\.git,\.repo}'
    alias egrep='egrep --color=auto --exclude-dir={\.svn,\.git,\.repo}'
fi

alias sshpw="ssh -o PubKeyAuthentication=No"
alias ssh-lth="ssh dt09gw1@login.student.lth.se"

alias ..='cd ..'
alias ....='cd ../..'
alias ......='cd ../../..'

alias sl=ls
alias ll="ls -lFh"
alias la="ls -Alh"
alias lh="ls -hl"
alias lah="ls -hal"
alias l="ls -CF"

alias rs="rsync --recursive"
alias rsp="rsync --progress --recursive"

alias maek=make
alias mkae=make
alias mkea=make
alias amke=make
alias akem=make
alias amek=make
alias meka=make

alias gcc-defines="gcc -dM -E - < /dev/null"
alias g++-defines="g++ -dM -E - < /dev/null"

alias latexmk="latexmk -pdf"

alias valgrind="valgrind --track-origins=yes --leak-check=full --show-reachable=yes"
alias helgrind="\valgrind --tool=helgrind"
alias qemu-arm="qemu-arm -L /usr/arm-linux-gnueabihf"

alias tgi=git
alias gti=git
alias gi=git
alias gt=git

alias svnst="svn st"
alias snv=svn

alias xfce-shortcuts="xfconf-query -c xfce4-keyboard-shortcuts -l -v | \
cut -d'/' -f4 | awk '{printf \"%-30s\", \$2 ; print \"\\t\" \$1 }' | \
sort | uniq"
