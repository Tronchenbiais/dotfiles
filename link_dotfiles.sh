#!/bin/bash
function make_link {
    target=$1 
    src=$2 
    if [ -e $src ]
    then
        echo -n "Replace $src? ('n' to keep, anything else overrides)"
        read replace
        if [ -n $replace -a $replace == "n" ]
        then
            echo "$src skipped"
            echo ""
            return 0
        fi
    fi
    mkdir -p `dirname $src` 
    ln -sf `pwd`/$target $src
    echo "$src linked"
    echo ""
}

make_link config/bashrc ~/.bashrc
make_link config/inputrc ~/.inputrc

make_link config/gitconfig ~/.gitconfig

make_link config/lxterminal.conf ~/.config/lxterminal/lxterminal.conf
make_link vim/vimrc ~/.config/nvim/init.vim
make_link config/vifmrc ~/.config/vifm/vifmrc

make_link config/xmonad-session-rc ~/.xmonad/xmonad-session-rc
make_link config/xmonad.hs ~/.xmonad/xmonad.hs
make_link config/xmobar.hs ~/.xmobarrc
make_link config/compton.conf ~/.config/compton.conf
make_link config/rofi.rasi ~/.config/rofi/config.rasi

make_link img/middleEarth.jpg ~/wallpaper/wallpaper
