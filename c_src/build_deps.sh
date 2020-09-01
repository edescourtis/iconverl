#!/bin/bash
cd $( cd $(dirname $0) ; pwd -P )
if [ -d "libiconv-1.16" ]; then
  rm -rf libiconv-1.16
fi
if [ -f "libiconv-1.16.tar.gz" ]; then
  rm libiconv-1.16.tar.gz
fi
wget 'https://ftp.gnu.org/pub/gnu/libiconv/libiconv-1.16.tar.gz'
tar xvzf libiconv-1.16.tar.gz
cd libiconv-1.16
./configure --enable-static
make -j9

