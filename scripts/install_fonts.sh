#!/usr/bin/env sh
# Installs some nice fonts.

# Create font directory and a working directory.
fonts=$XDG_DATA_HOME/fonts
tmp=$(mktemp --directory)
mkdir --parents \
      $HOME/.fonts \
      $fonts

# Download and install Adobe Source Code Pro.
url=https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip
wget $url --output-document=$tmp/source_code_pro.zip --quiet
unzip -q $tmp/source_code_pro.zip -d $tmp
find $tmp -name "*.otf" | xargs -I {} mv {} $fonts

# Download and install Fira Code.
url=https://github.com/tonsky/FiraCode/releases/download/1.204/FiraCode_1.204.zip
wget $url --output-document=$tmp/fira_code.zip --quiet
unzip -q $tmp/fira_code.zip -d $tmp
mv $tmp/ttf/*.ttf $fonts

# Download and install Monoid.
url="https://raw.githubusercontent.com/larsenwork/monoid/2db2d289f4e61010dd3f44e09918d9bb32fb96fd/Monoid.zip"
wget $url --output-document=$tmp/fira_code.zip --quiet
unzip -q $tmp/fira_code.zip -d $tmp
mv $tmp/*.ttf $fonts

# Download and install Iosevka.
url=https://github.com/be5invis/Iosevka/releases/download/v1.12.3/iosevka-pack-1.12.3.zip
wget $url --output-document=$tmp/fira_code.zip --quiet
unzip -q $tmp/fira_code.zip -d $tmp
mv $tmp/*.ttc $fonts

# Download and install Font-awesome icons.
url=http://fontawesome.io/assets/font-awesome-4.7.0.zip
wget $url --output-document=$tmp/font-awesome-4.7.0.zip --quiet
unzip -q $tmp/font-awesome-4.7.0.zip -d $tmp
mv $tmp/font-awesome-4.7.0/fonts/*.woff $fonts

# Remove the temporary directory.
rm -rf $tmp

# Update the font cache.
fc-cache
