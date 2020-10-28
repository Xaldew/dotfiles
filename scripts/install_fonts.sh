#!/usr/bin/env sh
# Installs some nice fonts.

# Create font directory and a working directory.
fonts=$XDG_DATA_HOME/fonts
mkdir --parents $fonts

install_fonts()
{
    # Download, extract and install the given font(s).
    url=${1:?"No URL given."}
    dst=${2:?"No destination given."}
    types=${3:-"*.otf"}
    tmp=$(mktemp --directory)
    file=$(basename ${url})
    wget ${url} --output-document=${tmp}/${file} --quiet
    case $file in
        *.[Zz][Ii][Pp]) unzip -q ${tmp}/${file} -d ${tmp} ;;
    esac
    find ${tmp} -name "${types}" | xargs -I {} mv {} ${dst}
    rm -rf ${tmp}
}

# Download and install Adobe Source Code Pro.
install_fonts \
    "https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip" \
    ${fonts} \
    "*.otf" &

# Download and install Fira Code.
install_fonts \
    "https://github.com/tonsky/FiraCode/releases/download/2/FiraCode_2.zip" \
    ${fonts} \
    "*.ttf" &

# Download the Fira Code Symbols font.
install_fonts \
    "https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip" \
    ${fonts} \
    "*.otf" &

# Download and install Monoid.
install_fonts \
    "https://raw.githubusercontent.com/larsenwork/monoid/2db2d289f4e61010dd3f44e09918d9bb32fb96fd/Monoid.zip" \
    ${fonts} \
    "*.ttf" &

wait

# Download and install Iosevka.
url="https://github.com/be5invis/Iosevka/releases/download/v3.0.1/pkg-iosevka-%s3.0.1.zip"
for v in "" slab- curly- curly-slab- aile- etoile- sparkle-; do
    pkg=$(printf "$url" $v)
    install_fonts "$pkg" ${fonts} "*.ttf" &
done
wait
for v in $(seq 1 14); do
    ss=$(printf "ss%02d-" $v)
    pkg=$(printf "$url" $ss)
    install_fonts "$pkg" ${fonts} "*.ttf" &
done
wait

# Download and install Hasklig font.
install_fonts \
    "https://github.com/i-tu/Hasklig/releases/download/1.1/Hasklig-1.1.zip" \
    ${fonts} \
    "*.otf" &

# Download and install the Fixedsys Excelsior fonts.
install_fonts \
    "https://github.com/kika/fixedsys/releases/download/v3.02.8/FSEX302-alt.ttf" \
    ${fonts} \
    "*.ttf" &
install_fonts \
    "https://github.com/kika/fixedsys/releases/download/v3.02.8/FSEX302.ttf" \
    ${fonts} \
    "*.ttf" &

# Install the DejaVu Sans Code font(s).
install_fonts \
    "https://github.com/SSNikolaevich/DejaVuSansCode/releases/download/v1.2.2/dejavu-code-ttf-1.2.2.zip" \
    ${fonts} \
    "*.ttf" &

# Download and install Font-awesome icons.
install_fonts \
    "https://use.fontawesome.com/releases/v5.7.2/fontawesome-free-5.7.2-desktop.zip" \
    ${fonts} \
    "*.otf" &


# Download Nerd-Font patched fonts.
install_fonts \
    "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip" \
    ${fonts} \
    "*.otf" &

install_fonts \
    "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/Hasklig.zip" \
    ${fonts} \
    "*.otf" &

install_fonts \
    "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/SourceCodePro.zip" \
    ${fonts} \
    "*.otf" &

install_fonts \
    "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/Iosevka.zip" \
    ${fonts} \
    "*.ttf" &

wait

# Update the font cache.
fc-cache -f
