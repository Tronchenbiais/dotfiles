This repo has a backup of my config files. It is to be used on debian.

Use the `install_software.sh` script to install the required packages from apt.

Use the `link_dotfiles.sh` script to link the real config files to the ones in
this repo.

The vim config is hosted on a separate repo, included as a submodule in this
directory. Use the following commands in the cloned repo to fetch it :

```
git submodule init
git submodule update
```

