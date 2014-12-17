#!/usr/bin/env sh
# Installs some nice fonts.

# Create font directory and a working directory.
mkdir -p $HOME/.fonts
tmpdir=$(mktemp --directory)

# Download and install Adobe Source Code Pro.
url=https://github.com/adobe-fonts/source-code-pro/archive/1.017R.zip
wget $url --output-document=$tmpdir/source_code_pro.zip --quiet
unzip -q $tmpdir/source_code_pro -d $tmpdir
cp $tmpdir/source-code-pro-1.017R/OTF/*.otf $HOME/.fonts

# Remove the temporary directory.
rm -rf $tmpdir

# Update the font cache.
fc-cache
