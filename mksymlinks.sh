#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/dotfiles                    # dotfiles directory
old_dir=~/dotfiles_old             # old dotfiles backup directory
files="vimrc nvimrc zshrc bashrc gitconfig gitignore_global gitignore irssi vimperatorrc emacs.d/init.el config/nvim/init.vim"
# list of files/folders to symlink in homedir

##########

# create dotfiles_old in homedir
echo "Creating $old_dir for backup of any existing dotfiles in home directory"
mkdir -p $old_dir
echo "...done"

# change to the dotfiles directory
echo "Changing to the $dir directory"
cd $dir
echo "...done"

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks
for file in $files; do
    # backup old dot files
    echo "Moving old [.$file] from home directory to $old_dir"
    mv ~/.$file $old_dir
    echo "Creating [.$file] symlink in home directory."
    if [ `echo $file | grep -c '/'` -gt 0 ]; then # if it has path like emacs.d/init.el
        paths=`echo $file | sed -n 's/\(.*\)\/.*\..*/\1/p'` # extract paths. i.e. emacs.d/init.el -> emacs.d
        echo "mkdir paths for ~/.$paths"
        mkdir -p ~/.$paths
    fi
    ln -s $dir/$file ~/.$file # ~/dotfiles/vimrc -> ~/.vimrc or ~/dotfiles/emacs.d/init.el -> ~/.emacs.d/init.el
done

# example
# if the file has not been listed in this folder, please run `dotfile .vimrc` first
# ~/dotfiles/vimrc -> ~/.vimrc
