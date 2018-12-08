FROM build-base:latest

MAINTAINER Björn Lindström <bkhl@elektrubadur.se>

ENV VERSION 2.8

ADD https://ftp.gnu.org/pub/gnu/ncurses/ncurses-6.1.tar.gz /build/ncurses-6.1.tar.gz
ADD https://github.com/libevent/libevent/releases/download/release-2.1.8-stable/libevent-2.1.8-stable.tar.gz /build/libevent-2.1.8-stable.tar.gz
ADD https://github.com/tmux/tmux/releases/download/2.8/tmux-2.8.tar.gz /build/tmux-2.8.tar.gz

ADD build.sh /build/build.sh

RUN sh /build/build.sh
