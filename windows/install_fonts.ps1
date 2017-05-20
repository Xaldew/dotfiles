# Install a number of font files for windows use.
$scriptPath  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$dotfilesDir = Split-Path -Parent $scriptPath

# Source common utilities.
. $dotfilesDir/windows/install_utils.ps1


# Download all font files.
$tmp = $(New-TemporaryDirectory).fullname
$wc = New-Object System.Net.WebClient

$url = "https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip"
$file = [io.path]::GetFileName($url)
$dst = [io.path]::combine($tmp, $file)
$wc.DownloadFile($url, $dst)
unzip $dst $tmp

$url = "https://github.com/tonsky/FiraCode/releases/download/1.204/FiraCode_1.204.zip"
$file = [io.path]::GetFileName($url)
$dst = [io.path]::combine($tmp, $file)
$wc.DownloadFile($url, $dst)
unzip $dst $tmp

$url = "https://raw.githubusercontent.com/larsenwork/monoid/2db2d289f4e61010dd3f44e09918d9bb32fb96fd/Monoid.zip"
$file = [io.path]::GetFileName($url)
$dst = [io.path]::combine($tmp, $file)
$wc.DownloadFile($url, $dst)
unzip $dst $tmp

$url = "https://github.com/be5invis/Iosevka/releases/download/v1.12.3/iosevka-pack-1.12.3.zip"
$file = [io.path]::GetFileName($url)
$dst = [io.path]::combine($tmp, $file)
$wc.DownloadFile($url, $dst)
unzip $dst $tmp


# Install all the OTF and TTF font files.
$FONTS = 0x14
$shell = New-Object -ComObject Shell.Application
$objFolder = $shell.Namespace($FONTS)
$files = Get-ChildItem $tmp -Recurse -Include *.ttf,*.otf
foreach($file in $files)
{
    if (!(Test-Path "C:/Windows/Fonts/$($file.name)"))
    {
        $objFolder.CopyHere($file.fullname)
    }
}

Remove-Item $tmp/* -recurse
