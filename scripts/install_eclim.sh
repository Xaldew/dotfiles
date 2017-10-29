#!/usr/bin/env sh

# Install Eclipse.
url="https://eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/oxygen/1a/eclipse-java-oxygen-1a-linux-gtk-x86_64.tar.gz&r=1"
wget --output-document=${objects_dir}/eclipse-java-oxygen-1a-linux-gtk-x86_64.tar.gz ${url}
tar xvzf ${objects_dir}/eclipse-java-oxygen-1a-linux-gtk-x86_64.tar.gz -C ${XDG_DATA_HOME}/
ln -s ${XDG_DATA_HOME}/eclipse/eclipse ${local_prefix_dir}/bin/eclipse

# Install the `eclim` server and client components.
url="https://github.com/ervandew/eclim/releases/download/2.7.0/eclim_2.7.0.jar"
wget --output-document=${objects_dir}/eclim_2.7.0.jar ${url}
java \
    -Dvim.skip=true \
    -Declipse.home=${XDG_DATA_HOME}/eclipse \
    -jar ${objects_dir}/eclim_2.7.0.jar \
    install
