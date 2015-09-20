function unzip($file, $dstPath)
{
    $shell = new-object -com shell.application
    New-Item -ItemType Directory -Force -Path $dstPath -WarningAction SilentlyContinue | Out-Null
    $shell.namespace($dstPath).copyhere($shell.namespace($file.FullName).items())
}

function Test-Administrator
{
    $user = [Security.Principal.WindowsIdentity]::GetCurrent();
    (New-Object Security.Principal.WindowsPrincipal $user).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)
}

function Test-CommandExists ($cmd)
{
    try
    {
        if (Get-Command $cmd)
        {
            return 1
        }
    }
    catch
    {
        return 0
    }
}
