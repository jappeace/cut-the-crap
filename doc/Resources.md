This is a document with random spells and links which I collected
to implement this.
It involves a lot of ffmpeg.

# FFMPEG
Track manipulation: https://superuser.com/questions/639402/convert-specific-video-and-audio-track-only-with-ffmpeg
Silence detection and cutting: https://stackoverflow.com/questions/36074224/how-to-split-video-or-audio-by-silent-parts/36077309#36077309

## Split media by silence
This one liner comes sortoff close to what we want to do, except not quite:

```shell
ffmpeg -i input.mkv -filter_complex "[0:a]silencedetect=n=-90dB:d=0.3[outa]" -map [outa] -f s16le -y /dev/null |& F='-aq 70 -v warning' perl -ne 'INIT { $ss=0; $se=0; } if (/silence_start: (\S+)/) { $ss=$1; $ctr+=1; printf "ffmpeg -nostdin -i input.mkv -ss %f -t %f $ENV{F} -y %03d.mkv\n", $se, ($ss-$se), $ctr; } if (/silence_end: (\S+)/) { $se=$1; } END { printf "ffmpeg -nostdin -i input.mkv -ss %f $ENV{F} -y %03d.mkv\n", $se, $ctr+1; }' | bash -x
```

## Combine streams
```shell
ffmpeg -i ./raster-uren-3-full.mp4 -i ~/raster-vids/silent.mp3 -filter_complex "[0:a][1:a]amerge=inputs=2[a]" -map 0:v -map "[a]" -c:v copy -c:a mp3 -ac 2 -shortest ./raster-music.mp4
```

https://stackoverflow.com/questions/12938581/ffmpeg-mux-video-and-audio-from-another-video-mapping-issue/12943003#12943003

## Loudness
```shell
ffmpeg -i ~/raster-vids/Birds_in_Flight.mp3 -filter:a "volume=0.05" ~/raster-vids/silent.mp3
```
https://www.maketecheasier.com/normalize-music-files-with-ffmpeg/

## Concatting videos
https://ffmpeg.org/faq.html#How-can-I-join-video-files_003f 

https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg


## Filter
There is a silence remove:
https://ffmpeg.org/ffmpeg-filters.html#silenceremove

It maybe worth investigating.

## Native bindings
Using native bindings instead of the cli interface can result
in a much more efficient program as we then can stream everything.
It's a lot harder to do though.

+ http://hackage.haskell.org/package/gstreamer
+ http://hackage.haskell.org/package/ffmpeg-light

# Matroska
This is the container for a video. It has one or more audio tracks and a
video track.
We'll try using this first cause it's default in obs
(I know atm nothing about codecs, I imagine this will change soon).

+ https://www.matroska.org/technical/specs/index.html
+ https://github.com/vi/HsMkv
