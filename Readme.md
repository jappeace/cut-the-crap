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
It is possible to specify one audio output as speech track.
This will be used to for silence detection only.
The result is very precize silence detection.

### Seperate music track
Another track would be background and won't be modified at all.
In the end it just get's cut of how far it is.

This way we get good music and interesting stream.
Another idea is to remix an entirely different source of music
into the video, so we can play copyrighted music on stream
and youtube friendly music on youtube.

# TODO

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
