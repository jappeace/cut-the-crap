[![Jappiejappie](https://img.shields.io/badge/twitch.tv-jappiejappie-purple?logo=twitch&style=for-the-badge)](https://www.twitch.tv/jappiejappie)

> Bless This Mess

This is an automatic video editing program and splitting
program.
At the moment video editing is done with [jumpcut](https://github.com/carykh/jumpcutter).
Splitting is done with ffmpeg.

Youtube has different requirements from streams then twitch does.
We for example want to make the videos be chopped up into
small segments.
We also should cut out boring parts.
Jumpcut has solved that problem partly and this program
leverages that.

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
I'm much more productive in haskell,
therefore I chose to integrate with,
and eventually replace jumpcutter.
On stream we've assesed most of the functionality is basically
ffmpeg.
Haskell also opens up the ability to do direct native ffmpeg
integration.

One glaring limitation I've  encountered for jumpcutter is that
it can't handle larger video files (2 hour 30 mintus +).
Scipy throws an exception complaining the wav is to big.
Currently I'm just splitting bigger videos with ffmpeg by doing:

```shell
ffmpeg -i input.mp4 -c copy -map 0 -segment_time 02:30:00 -f segment -reset_timestamps 1 output%03d.mp4
```

It also appears like jumpcutter is unmaintained.

# Design
This project is mostly a wrapper around ffmpeg and jumpcut,
which is also mostly a wrapper around ffmpeg.

> Wrapception is unix philosphy
>
> \- Lumie

# TODO

## Track hackery

+ It should be possible to specify one audio output as command track,
  eg it will be possible to use that to detect interesting parts.
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
## FFMPEG
Track manipulation: https://superuser.com/questions/639402/convert-specific-video-and-audio-track-only-with-ffmpeg
Silence detection and cutting: https://stackoverflow.com/questions/36074224/how-to-split-video-or-audio-by-silent-parts/36077309#36077309

## Native bindings
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

