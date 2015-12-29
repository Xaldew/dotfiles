# Install all Cygwin packages used during regular development.
$scriptPath  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$dotfilesDir = Split-Path -Parent $scriptPath

# Source common utilities.
. $dotfilesDir/windows/install_utils.ps1


if (!(Test-CommandExists cyg-get))
{
    Write-Host "\"cyg-get\" command not found. Please install it first."
    Exit 1
}


$packages = @(
    "tmux",
    "cygutils-extra",
    "procps",
    "emacs-w32",
    "zip",
    "unzip",
    "xz",
    "openssh",
    "ca-certificates",
    "git",
    "subversion",
    "mercurial",
    "make",
    "curl",
    "wget",
    "python",
    "hunspell",
    "ncurses",
    "libncurses-devel"
    "imagemagick",
    "autoconf",
    "automake",
    "cmake",
    "gcc",
    "clang",
    "aspell",
    "aspell-sv",
    "aspell-en",
    "xclip"
)

cyg-get $packages
