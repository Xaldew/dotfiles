#!/usr/bin/env sh
# Installs some nice fonts.

# Create font directory and a working directory.
mkdir --parents $HOME/.fonts
tmpdir=$(mktemp --directory)

# Download and install Adobe Source Code Pro.
url=https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip
wget $url --output-document=$tmpdir/source_code_pro.zip --quiet
unzip -q $tmpdir/source_code_pro.zip -d $tmpdir
find $tmpdir -name "*.otf" | xargs -I {} mv {} $HOME/.fonts

# Download and install Fira Code.
url=https://github.com/tonsky/FiraCode/releases/download/1.204/FiraCode_1.204.zip
wget $url --output-document=$tmpdir/fira_code.zip --quiet
unzip -q $tmpdir/fira_code.zip -d $tmpdir
mv $tmpdir/ttf/*.ttf $HOME/.fonts

# Download and install Monoid.
url="https://raw.githubusercontent.com/larsenwork/monoid/2db2d289f4e61010dd3f44e09918d9bb32fb96fd/Monoid.zip"
wget $url --output-document=$tmpdir/fira_code.zip --quiet
unzip -q $tmpdir/fira_code.zip -d $tmpdir
mv $tmpdir/*.ttf $HOME/.fonts

# Download and install Iosevka.
url=https://github.com/be5invis/Iosevka/releases/download/v1.12.3/iosevka-pack-1.12.3.zip
wget $url --output-document=$tmpdir/fira_code.zip --quiet
unzip -q $tmpdir/fira_code.zip -d $tmpdir
mv $tmpdir/*.ttc $HOME/.fonts

# Download and install Font-awesome icons.
url=http://fontawesome.io/assets/font-awesome-4.7.0.zip
wget $url --output-document=$tmpdir/font-awesome-4.7.0.zip --quiet
unzip -q $tmpdir/font-awesome-4.7.0.zip -d $tmpdir
mv $tmpdir/font-awesome-4.7.0/fonts/*.woff $HOME/.fonts

# Remove the temporary directory.
rm -rf $tmpdir

# Update the font cache.
fc-cache
