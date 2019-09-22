#!/usr/bin/env bash

dotfile_dir=~/dotfiles
dotfile () {
    set -e
    echo "dotfile for [ $1 ] now..."
    dot_file=$1
    swap="$1.swap" # swap file saved to dotfiles/
    file_name=`echo $dot_file | sed -n 's/[^\.]*\.\(.*\)/\1/p'` # e.g. /Users/whoami/.vimrc -> vimrc; test for `[^\.]*` /Users/whoami/.xxx.conf -> xxx.conf
    no_dot="$dotfile_dir/$file_name"

    echo "1. swap $dot_file to $swap"
    cp $dot_file $swap

    echo "2. move $swap to $no_dot"
    mv $swap $no_dot

    echo "3. replace $no_dot with $dot_file"
    ln -sf $no_dot $dot_file 

    echo "done [ $dot_file ] -> [ $no_dot ]."
}

dotfile "$@"
