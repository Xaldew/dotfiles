#!/usr/bin/env bash
# Installs some nice fonts.

# Create font directory and a working directory.
mkdir -p $HOME/.fonts
tmpdir=$(mktemp --directory)


# Download and install Adobe Source Code Pro.
url=http://sourceforge.net/projects/sourcecodepro.adobe
url+=/files/SourceCodePro_FontsOnly-1.017.zip/download
wget $url --output-document=$tmpdir/source_code_pro.zip --quiet
unzip -q $tmpdir/source_code_pro -d $tmpdir
cp $tmpdir/SourceCodePro_FontsOnly-1.017/OTF/SourceCodePro* $HOME/.fonts


# Remove the temporary directory.
rm -rf $tmpdir

# Update the font cache.
fc-cache
