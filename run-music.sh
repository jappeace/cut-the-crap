#! /bin/sh

set -xe

echo "script that will process a folder with streams";

files=$(ls $1);
for i in $files; do 
    echo "Processing "$i;
    cabal new-run exe -- \
            --inFile $1/$i \
            --outFile $i \
            --voiceTrack 2 \
            --musicTrack 3;
done;
