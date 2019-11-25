[![Jappiejappie](https://img.shields.io/badge/twitch.tv-jappiejappie-purple?logo=twitch&style=for-the-badge)](https://www.twitch.tv/jappiejappie)
[![Jappiejappie](https://img.shields.io/badge/youtube-jappieklooster-red?logo=youtube&style=for-the-badge)](https://www.youtube.com/channel/UCQxmXSQEYyCeBC6urMWRPVw)
[![Build status](https://img.shields.io/travis/jappeace/cut-the-crap?style=for-the-badge)](https://travis-ci.org/jappeace/cut-the-crap/builds/)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord&style=for-the-badge)](https://discord.gg/Hp4agqy)

> Bless This Mess

Cut the crap is an automatic video editing program for streamers.
It can cut out uninteresting parts by detecting silences.
This was inspired by [jumpcutter](https://github.com/carykh/jumpcutter),
where this program can get better quality results
by using an (optional) dedicated microphone track.
This prevents cutting of [quieter consonants](https://youtu.be/DQ8orIurGxw?t=675)
for example.
Using ffmpeg more efficiently also produces faster results and
is less error prone.

Youtube has different requirements from streams then twitch does.
We want to cut out boring parts.
Jumpcut has solved that problem partly and this program
builds on top of that idea.
At the moment we use ffmpeg for silence detection, 
then we do some maths to figure out which segments are sounded,
which is combined into the output video.

In the future we will add support for a music track
which will not be chopped up.

# Use case
I'm using this program to record my [stream](https://www.twitch.tv/jappiejappie)
and upload it to my
[youtube channel](https://www.youtube.com/channel/UCQxmXSQEYyCeBC6urMWRPVw).

Feel free to use or modify this program however you like.
Pull requests are appreciated.

## Why not to extend jumpcutter directly?
I wish to build out this idea more to essentially
make all streams look like human edited youtube videos.
Although I'm familiar with python,
I (am or feel) more productive in haskell,
therefore I chose to integrate with,
and eventually replace jumpcutter.
On stream we've assesed most of the functionality is basically
ffmpeg.
Haskell also opens up the ability to do direct native ffmpeg
integration.

One glaring limitation I've encountered with jumpcutter is that
it can't handle larger video files (2 hour 30 mintus +).
Scipy throws an exception complaining the wav is to big.
Since this program doesn't use scipy it doesn't have that issue.

It also appears like jumpcutter is unmaintained.

# Design
This project is mostly a wrapper around ffmpeg.
We use haskell for shell programing.

# DONE

## Track hackery

+ It should be possible to specify one audio output as command track,
  eg it will be possible to use that to detect interesting parts.

# TODO

## Track hackery

+ Another track would be background and won't be accelerated at all.
  In the end it just get's cut of how far it is.

This way we get good music and interesting stream.
Another idea is to remix an entirely different source of music
into the video, so we can play copyrighted music on stream
and youtube friendly music on youtube.

## Speech recognition
It should be rather easy to hook up for example http://kaldi-asr.org/doc/

This is a howto: https://towardsdatascience.com/how-to-start-with-kaldi-and-speech-recognition-a9b7670ffff6

With that we can try to for example cut out keyboard clacking
and cut out stop words like 'uhm'.

We can also start doing subject detection with a transcription in place.
And use the transcription to add subtitles to youtube.

## Youtube
The next priority will be automated uploading, possibly with:
https://github.com/tokland/youtube-upload
This will make scheduling easier,
that's a lot of clicking work at the moment.
Where the publish date starts at $DATE and is incremented by $SEG_HOUR.

I also looked at
[gogol yooutube](http://hackage.haskell.org/package/gogol-youtube).
But I couldn't figure out auth.
Maybe we should use the servant type instead?

Youtube has a [quota](https://developers.google.com/youtube/v3/getting-started#quota)
of 10 000,
which means you can only uplaod 4 vidoes with api's a day.
I have to try this before I can see if it works.

# Resoures
```shell
ffmpeg -i input.mkv -filter_complex "[0:a]silencedetect=n=-90dB:d=0.3[outa]" -map [outa] -f s16le -y /dev/null |& F='-aq 70 -v warning' perl -ne 'INIT { $ss=0; $se=0; } if (/silence_start: (\S+)/) { $ss=$1; $ctr+=1; printf "ffmpeg -nostdin -i input.mkv -ss %f -t %f $ENV{F} -y %03d.mkv\n", $se, ($ss-$se), $ctr; } if (/silence_end: (\S+)/) { $se=$1; } END { printf "ffmpeg -nostdin -i input.mkv -ss %f $ENV{F} -y %03d.mkv\n", $se, $ctr+1; }' | bash -x
```
## FFMPEG
Track manipulation: https://superuser.com/questions/639402/convert-specific-video-and-audio-track-only-with-ffmpeg
Silence detection and cutting: https://stackoverflow.com/questions/36074224/how-to-split-video-or-audio-by-silent-parts/36077309#36077309


### Combine streams
```shell
ffmpeg -i ./raster-uren-3-full.mp4 -i ~/raster-vids/silent.mp3 -filter_complex "[0:a][1:a]amerge=inputs=2[a]" -map 0:v -map "[a]" -c:v copy -c:a mp3 -ac 2 -shortest ./raster-music.mp4
```

https://stackoverflow.com/questions/12938581/ffmpeg-mux-video-and-audio-from-another-video-mapping-issue/12943003#12943003

### Loudness
```shell
ffmpeg -i ~/raster-vids/Birds_in_Flight.mp3 -filter:a "volume=0.05" ~/raster-vids/silent.mp3
```
https://www.maketecheasier.com/normalize-music-files-with-ffmpeg/

### Concatting videos
https://ffmpeg.org/faq.html#How-can-I-join-video-files_003f 

https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg


### Filter
There is a silence remove:
https://ffmpeg.org/ffmpeg-filters.html#silenceremove

It maybe worth investigating.

### Native bindings
Using native bindings instead of the cli interface can result
in a much more efficient program as we then can stream everything.
It's a lot harder to do though.

+ http://hackage.haskell.org/package/gstreamer
+ http://hackage.haskell.org/package/ffmpeg-light

## Matroska
This is the container for a video. It has one or more audio tracks and a
video track.
We'll try using this first cause it's default in obs
(I know atm nothing about codecs, I imagine this will change soon).

+ https://www.matroska.org/technical/specs/index.html
+ https://github.com/vi/HsMkv

# Plan of approach

1. Figure out how to rip out the music from stream

Select the right track with map:
ffmpeg -i ~/streams/towards-automated-video-editing.mkv -map 0:1 towards-automated-music.mp3


2. Experiment w/ Merge two streams together

ffmpeg -i ~/streams/towards-automated-video-editing.mkv -ss 0 -t 120 -i ~/US_Army_Blues_-_04_-_Not_On_The_Bus.mp3 -ss 0 -t 120 -map 0:0 -map 1:0 towards.mkv

3. Do the detection of silence on a stream

ffmpeg -i "/home/jappie/streams/towards-automated-video-editing.mkv" -map 0:2 -filter:a silencedetect=noise=-30dB:d=0.5 -f null - 2> vol.txt

4. Merge silences w/ music

.. TODO


