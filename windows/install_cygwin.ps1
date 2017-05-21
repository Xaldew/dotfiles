# Install all Cygwin packages used during regular development.
$scriptPath  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$dotfilesDir = Split-Path -Parent $scriptPath

# Source common utilities.
. $dotfilesDir/windows/install_utils.ps1


if (!(Test-CommandExists "cyg-install.exe"))
{
    $url = "https://cygwin.com/setup-x86_64.exe"
    $dst = "$HOME/.local/bin/cyg-install.exe"
    New-Item -Path $HOME/.local/bin -ItemType Directory -Force | Out-Null
    $wc = New-Object System.Net.WebClient
    $wc.DownloadFile($url, $dst)
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
    "libMagick-devel",
    "libMagickCore6_2",
    "libgif-devel",
    "libtiff-devel",
    "libjpeg-devel",
    "libXpm-devel",
    "libXpm-noX-devel",
    "libxml2-devel",
    "librsvg2-devel",
    "libfreetype-devel",
    "autoconf",
    "automake",
    "cmake",
    "gcc",
    "clang",
    "aspell",
    "aspell-sv",
    "aspell-en",
    "xclip"
) -join ","

cyg-install /q -P $packages
