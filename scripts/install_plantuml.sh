#!/usr/bin/env sh
url=http://sourceforge.net/projects/plantuml/files/plantuml.jar/download
wget --quiet $url --output-document=$local_prefix_dir/bin/plantuml
chmod u+x $local_prefix_dir/bin/plantuml
