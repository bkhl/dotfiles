#!/bin/sh

tmux attach-session -t main || tmux new-session -s main
