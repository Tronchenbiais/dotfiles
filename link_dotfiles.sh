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

make_link bashrc ~/.bashrc
make_link inputrc ~/.inputrc

make_link gitconfig ~/.gitconfig

make_link lxterminal.conf ~/.config/lxterminal/lxterminal.conf
make_link vimrc ~/.config/nvim/init.vim
make_link vifmrc ~/.config/vifm/vifmrc

make_link xsessionrc ~/.xsessionrc
make_link xmonad.hs ~/.xmonad/xmonad.hs
make_link compton.conf ~/.config/compton.conf

make_link img/middleEarth.jpg ~/wallpaper/middleEarth.jpg
