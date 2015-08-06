#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/dotfiles                    # dotfiles directory
old_dir=~/dotfiles_old             # old dotfiles backup directory
files="vimrc nvimrc zshrc bashrc gitconfig gitignore_global gitignore irssi vimperatorrc"    # list of files/folders to symlink in homedir

##########

# create dotfiles_old in homedir
echo "Creating $old_dir for backup of any existing dotfiles in ~"
mkdir -p $old_dir
echo "...done"

# change to the dotfiles directory
echo "Changing to the $dir directory"
cd $dir
echo "...done"

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks
for file in $files; do
    echo "Moving old [.$file] from home directory to $old_dir"
    mv ~/.$file $old_dir
    echo "Creating [.$file] symlink in home directory."
    ln -s $dir/$file ~/.$file
done

# example
# ~/dotfiles/vimrc -> ~/.vimrc
