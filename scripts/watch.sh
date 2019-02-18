#! /bin/sh


set -x

(sleep 1 && touch app) & # makes it go the first time
# TODO make this with tmux http://man7.org/linux/man-pages/man1/tmux.1.html
# we can leave the nix shells open in that for faster builds
# (loading currenlty takes quite a bit of time)
while nix-shell --pure -p pkgs.inotify-tools --run  "inotifywait -r -e modify -e create -e attrib ./src ./app ./test";
do
    make run-in-shell RUN="make build"
done
