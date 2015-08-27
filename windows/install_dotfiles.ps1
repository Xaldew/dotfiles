$scriptPath  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$dotfilesDir = Split-Path -Parent $scriptPath

Write-Host "Home location: $HOME"
Write-Host "AppData location: $Env:APPDATA"
Write-Host "Script location: $scriptPath"
Write-Host "Dotfiles location: $dotfilesDir"

# Copy all Bash configuration.
Copy-Item -Path $dotfilesDir/configs/bashrc -Destination $HOME/.bashrc -Force
Copy-Item -Path $dotfilesDir/configs/bash_aliases -Destination $HOME/.bash_aliases -Force

# Copy all SSH configuration.
If (!(Test-Path -Path $HOME/.ssh -PathType Container))
{
    New-Item -Path $HOME/.ssh -ItemType Directory | Out-Null
}
Copy-Item -Path $dotfilesDir/configs/ssh_config -Destination $HOME/.ssh/config -Force

# Copy all Git configuration.
Copy-Item -Path $dotfilesDir/configs/gitconfig -Destination $HOME/.gitconfig -Force
Copy-Item -Path $dotfilesDir/configs/gitignore -Destination $HOME/.gitignore -Force


# Copy all Emacs configuration.
# Note that it is placed in APPDATA instead of HOME since that's where Emacs
# look for configuration files by default.
