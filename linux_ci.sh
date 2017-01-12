#/bin/bash
set -e
stack setup
stack build --test
BIN_LOCATION=`stack path --local-install-root`
rm -rf pkg
mkdir -p pkg
cp $BIN_LOCATION/bin/* pkg
cd pkg
zip hastile_linux.zip *
