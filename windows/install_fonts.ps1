# Install a number of font files for windows use.
$scriptPath  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$dotfilesDir = Split-Path -Parent $scriptPath

# Source common utilities.
. $dotfilesDir/windows/install_utils.ps1

# Download all font files.
function Install-Fonts([string]$url, [string]$type)
{
    $tmp = $(New-TemporaryDirectory).fullname
    $wc = New-Object System.Net.WebClient
    $ext = [io.path]::GetExtension($url)
    $file = [io.path]::GetFileName($url)
    $dst = [io.path]::combine($tmp, $file)
    $wc.DownloadFile($url, $dst)
    if ($ext -ieq ".zip")
    {
        unzip $dst $tmp
    }

    # Install all the encountered font files.
    $fonts = 0x14
    $shell = New-Object -ComObject Shell.Application
    $objFolder = $shell.Namespace($fonts)
    $files = Get-ChildItem $tmp -Recurse -Include $type
    foreach($file in $files)
    {
        if (!(Test-Path "C:/Windows/Fonts/$($file.name)"))
        {
            $objFolder.CopyHere($file.fullname)
        }
    }
    Remove-Item $tmp/* -Recurse
}

$fonts = @(
    @{
        url="https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip";
        types="*.otf";
    },
    @{
        url="https://github.com/tonsky/FiraCode/releases/download/1.204/FiraCode_1.204.zip";
        types="*.ttf";
    },
    @{
        url="https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip";
        types="*.otf"
    },
    @{
        url="https://raw.githubusercontent.com/larsenwork/monoid/2db2d289f4e61010dd3f44e09918d9bb32fb96fd/Monoid.zip";
        types="*.ttf";
    },
    @{
        url="https://github.com/be5invis/Iosevka/releases/download/v1.12.3/iosevka-pack-1.12.3.zip";
        types="*.ttc";
    },
    @{
        url="https://github.com/i-tu/Hasklig/releases/download/1.1/Hasklig-1.1.zip";
        types="*.otf";
    },
    @{
        url="https://github.com/SSNikolaevich/DejaVuSansCode/releases/download/v1.2.2/dejavu-code-ttf-1.2.2.zip";
        types="*.ttf";
    },
    @{
        url="https://github.com/kika/fixedsys/releases/download/v3.02.8/FSEX302-alt.ttf";
        types="*.ttf";
    },
    @{
        url="https://github.com/kika/fixedsys/releases/download/v3.02.8/FSEX302.ttf";
        types="*.ttf";
    },
    @{
        url="http://fontawesome.io/assets/font-awesome-4.7.0.zip";
        types="*.otf";
    }
)

foreach ($font in $fonts)
{
    Install-Fonts $font.url $font.types
}
