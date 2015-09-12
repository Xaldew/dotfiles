# Compute script and dotfiles base directory.
$scriptPath  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$dotfilesDir = Split-Path -Parent $scriptPath


# Add HOME as an environment variable.
[Environment]::SetEnvironmentVariable("HOME", $HOME, "User")


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
Copy-Item -Path $dotfilesDir/windows/gitconfig -Destination $HOME/.gitconfig -Force
Copy-Item -Path $dotfilesDir/configs/gitignore -Destination $HOME/.gitignore -Force


# Copy all Emacs configuration.
# Note that it is placed in both APPDATA and HOME since that is where Emacs
# looks for configuration files by default.
Copy-Item $dotfilesDir/configs/emacs.d/ -Destination $HOME/.emacs.d/ -Force -Recurse
Copy-Item $dotfilesDir/configs/emacs.d/ -Destination $Env:APPDATA/.emacs.d/ -Force -Recurse


# Copy all AutoHotkey configuration.
