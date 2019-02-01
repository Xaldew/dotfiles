# Install Leiningen for Clojure development.
if [ ! -x $local_prefix_dir/bin/lein ]; then
    url="https://raw.githubusercontent.com/technomancy/"
    url=$url"leiningen/stable/bin/lein"
    wget --quiet $url --output-document=$local_prefix_dir/bin/lein
    chmod u+x $local_prefix_dir/bin/lein
fi
