$scriptPath  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$dotfilesDir = Split-Path -Parent $scriptPath

# Source common utilities.
. $dotfilesDir/windows/install_utils.ps1


if (!(Test-Administrator))
{
    Write-Host "This script needs to be run as an administrator."
    Exit 1
}

# Install Chocolatey if not already installed.
if (!(Test-CommandExists Choco))
{
    Invoke-Expression ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1')) | Out-Null
}

$packages = @(
    "7zip",
    "firefox",
    "flashplayerplugin",
    "adobereader",
    "emacs",
    "hunspell",
    "languagetool",
    "inkscape",
    "autohotkey",
    "steam",
    "battle.net",
    "vlc"
)

Write-Host "Installing: $packages"
choco install $packages
