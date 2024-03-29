# Install the basic packages used in my Windows installations.
$scriptPath  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$dotfilesDir = Split-Path -Parent $scriptPath

# Source common utilities.
. $dotfilesDir/windows/install_utils.ps1

if (!(Test-Administrator))
{
    Write-Host "This script needs to be run as an administrator."
    Exit 1
}

# Install the NuGet and Chocolatey package providers.
Get-PackageProvider -Name nuget
Get-PackageProvider -Name chocolatey
Set-PackageSource -Name chocolatey -Trusted

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
    "vlc",
    "skype",
    "f.lux",
    "python2",
    "python3"
)

foreach ($pkg in $packages)
{
    Write-Host "Installing $pkg"
    Install-Package $pkg
}

# Add the Chocolatey binary folder to Path.
$newPath = [io.path]::combine($Env:ChocolateyPath, "bin")
[Environment]::SetEnvironmentVariable("Path", $Env:Path + ";$newPath", "User")
