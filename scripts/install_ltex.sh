#!/usr/bin/env bash

name="ltex-ls-15.2.0-linux-x64.tar.gz"
installname="ltex-ls-15.2.0"
url="https://github.com/valentjn/ltex-ls/releases/download/15.2.0/ltex-ls-15.2.0-linux-x64.tar.gz"
wget --output-document=${objects_dir}/${name} ${url}
tar xvzf ${objects_dir}/${name} -C ${XDG_DATA_HOME}/
ln -fs ${XDG_DATA_HOME}/${installname}/bin/ltex-ls ${local_prefix_dir}/bin/ltex-ls
ln -fs ${XDG_DATA_HOME}/${installname}/bin/ltex-cli ${local_prefix_dir}/bin/ltex-cli
