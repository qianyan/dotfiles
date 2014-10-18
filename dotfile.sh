dotfile () {
    set -e
    echo "dotfile now..."
    old_file="$1.dotfile"
    dot_file=~/dotfiles/${1:1}
    cp -R $1 $old_file
    mv $old_file $dot_file
    ln -sF $dot_file $1
    echo "dotfile migration done."
}
