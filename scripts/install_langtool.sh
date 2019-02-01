# Install LanguageTool for spell- and grammarchecking.
if command -v java >/dev/null 2>&1 && \
       [ ! -d $local_prefix_dir/bin/languagetool ]; then
    dest=$local_prefix_dir/bin/languagetool
    mkdir --parents $dest
    tmpdir=$(mktemp --directory)
    url=https://languagetool.org/download/LanguageTool-stable.zip
    wget --quiet $url --output-document=$tmpdir/languagetool.zip
    zip=$tmpdir/languagetool.zip

    # Extract zip and strip toplevel directories.
    unzip -q -d "$dest" "$zip"
    for f in "$dest"/*;
    do
        mv $f/* "$dest"
        rmdir $f
    done
    rm -r $tmpdir
fi
