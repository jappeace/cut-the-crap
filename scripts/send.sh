#! /bin/sh


set -x

(sleep 1 && touch backend/backend.cabal) & # makes it go the first time
# TODO make this with tmux http://man7.org/linux/man-pages/man1/tmux.1.html
# we can leave the nix shells open in that for faster builds
# (loading currenlty takes quite a bit of time)

# TODO use: https://github.com/schell/steeloverseer#readme
# I think it does this better
while nix-shell --pure -p pkgs.inotify-tools --run  "inotifywait -r -e modify -e create ./backend ./common ./frontend ./test ./packages ./experiment ./opensource ./default.nix";
do
    ssh jappie@192.168.0.155 "mkdir -p /tmp/raster"
    rsync --delete --exclude 'dist-newstyle' --exclude '.git' --exclude 'dist-ghcjs' --rsync-path=/nix/store/5my64lkzh8da3j51cjvpyqn9680vxq0i-system-path/bin/rsync -avz . jappie@192.168.0.155:/tmp/raster
    sleep 1
    rsync --delete --exclude 'dist-newstyle' --exclude '.git' --exclude 'dist-ghcjs' --rsync-path=/nix/store/5my64lkzh8da3j51cjvpyqn9680vxq0i-system-path/bin/rsync -avz . jappie@192.168.0.155:/tmp/raster
done
