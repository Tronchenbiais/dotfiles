#!/bin/bash
function make_link {
    target=$1 
    src=$2 
    if [ -e $src ]
    then
        echo "$src exists, skipping..."
    else
        mkdir -p `dirname $src` 
        ln -s `pwd`/$target $src
        echo "$src linked"
    fi
}

# ln -s `pwd`/bashrc ~/.bashrc ##  NOT IMPLEMENTED
# ln -s `pwd`/inputrc ~/.inputrc ##  NOT IMPLEMENTED

make_link vimrc ~/.vimrc
make_link vifmrc ~/.config/vifm/vifmrc
make_link xmonadrc ~/.xmonad/xmonad.hs

make_link xsessionrc ~/.xsessionrc
