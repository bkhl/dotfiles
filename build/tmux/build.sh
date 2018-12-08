#!/bin/sh

set -e
set -x

export PREFIX=/usr/local
export TERMINFO=/lib/terminfo

# ncurses
cd /build
tar xzf 'ncurses-6.1.tar.gz'
cd 'ncurses-6.1'
./configure --enable-static --prefix="$PREFIX"
make -j16 install

# libevent
cd /build
tar xzf 'libevent-2.1.8-stable.tar.gz'
cd 'libevent-2.1.8-stable'
./configure --enable-static --prefix="$PREFIX"
make -j16 install

# tmux
cd /build
tar xzf "tmux-$VERSION.tar.gz"
cd tmux-"$VERSION"
./configure --enable-static --prefix="$PREFIX" CPPFLAGS="-I$PREFIX/include -I$PREFIX/include/ncurses" LDFLAGS="-static -L$PREFIX/include -L$PREFIX/include/ncurses -L$PREFIX/lib"
make -j16
ln tmux /build
