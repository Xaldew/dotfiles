#!/usr/bin/env sh
# Install Boodler and a collection of .boop-files.

cd $objects_dir
wget --quiet http://boodler.org/dl/Boodler-2.0.4.tar.gz
tar xf Boodler-2.0.4.tar.gz
rm Boodler-2.0.4.tar.gz
cd Boodler-2.0.4
python setup.py build
python setup.py install --prefix $local_prefix_dir

mkdir --parents $HOME/.boodler/Download
cd $HOME/.boodler/Download
wget --no-directories --quiet --recursive --no-parent --accept "*.boop" \
     http://boodler.org/lib/

for f in *.boop; do
    boodle-mgr install $f > /dev/null
done
