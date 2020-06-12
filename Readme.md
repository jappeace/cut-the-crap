![logo](doc/logo.png)

[![Jappiejappie](https://img.shields.io/badge/twitch.tv-jappiejappie-purple?logo=twitch)](https://www.twitch.tv/jappiejappie)
[![Jappiejappie](https://img.shields.io/badge/Youtube-jappieklooster-red?logo=Youtube)](https://www.Youtube.com/channel/UCQxmXSQEYyCeBC6urMWRPVw)
[![Build status](https://img.shields.io/travis/jappeace/cut-the-crap)](https://travis-ci.org/jappeace/cut-the-crap/builds/)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)

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

# Install

## Nix/Nixos
+ Apply [overlay](https://nixos.wiki/wiki/Overlays) found [here](https://github.com/jappeace/cut-the-crap/tree/master/overlay).
+ Run `nix-env -iA cut-the-crap` or add to systemPackages.
+ simply run `cut-the-crap` to display usage instructions.

## Ubuntu
Download the .deb file from the release page.
Install with:

```shell
apt install ffmpeg
dpkg -i ./cut-the-crap_1.0.0_amd64.deb 
```

Execute with:
```shell
cut-the-crap
```
# Usage notes

## Noise gate
Make sure to record with a noise gate on your microphone.
This will cut out background buzzing and allow you to use a more aggressive
threshold on noise detection.

## OBS tracks

Setup OBS so that you record the microphone and the desktop audio
on [separate tracks](https://obsproject.com/forum/resources/obs-studio-high-quality-recording-and-multiple-audio-tracks.221/).
In my own setup I have track 1 for combining all audio, track 2 for just the microphone and track 3 for desktop audio.
Then I can use:

```shell
    cut-the-crap --inputFile ./recordFromObs.mkv --outputFile ./someOut.mkv --voiceTrack 2 --musicTrack 3
```

So we throw away track 1, we use track 2 for silence detection, and track 3 get's mixed in after cutting is complete.
If you don't want music being mixed back into the result,
for example for further editing,
you can also leave that argument out.
I did this for example to mix back in the music of the original file later.

## Test data
It maybe a bit awkward to record yourself just for testing data.
To get some easy test date we can use youtube-dl, and make it a bit shorter with ffmpeg,
for example:

```shell
youtube-dl "https://www.youtube.com/watch?v=kCpQ4aTzlis" && ffmpeg -i "Opening Ceremony & 'Languages all the way down' by Rob Rix - ZuriHac 2020-kCpQ4aTzlis.mkv" -t 00:20:00.00 -c copy input.mkv
```

# Use case
I'm using this program to record my [stream](https://www.twitch.tv/jappiejappie)
and upload it to my
[Youtube channel](https://www.Youtube.com/channel/UCQxmXSQEYyCeBC6urMWRPVw).

The concrete result is that your audience retention percentage will go up since the videos
will be shorter, and more engaging.
Sometimes on stream I have intro screens for example which completely get removed,
and other times I'm simply thinking.
Reducing videos by 30% is not uncommon in my case, which means by default
30% more retention.
You could even decide to edit after that which means you have to spend less time
on cutting out silences and more time on making it look cool.

Feel free to use or modify this program however you like.
Pull requests are appreciated.

# Features

## Track based silence detection
It is possible to specify one audio output as speech track.
This will be used to for silence detection only.
The result is very precise silence detection.

## Separate music track
Another track would be background and won't be modified at all.
In the end it just get's cut of how far it is.

This way we get good music and interesting stream.
Another idea is to remix an entirely different source of music
into the video, so we can play copyrighted music on stream
and Youtube friendly music on Youtube.

# Design
This project is mostly a wrapper around ffmpeg.
We use Haskell for shell programming.

The [shelly](http://hackage.haskell.org/package/shelly) library was chosen in support of shell programming.
Originally we used [turtle](http://hackage.haskell.org/package/turtle),
but that library is much more complicated to use because it assumes you
want to do stream programming,
creating several unexpected bugs.
So we replaced it with shelly and noticabally reduced code complexity.
Now it's truly a 'dumb' wrapper around ffmpeg.

## Why not to extend jumpcutter directly?
I wish to build out this idea more to essentially
make all streams look like human edited Youtube videos.
Although I'm familiar with python,
I (am or feel) more productive in haskell,
therefore I chose to integrate with,
and eventually replace jumpcutter.
On stream we've determined most of the functionality is basically
ffmpeg.
Haskell also opens up the ability to do direct native ffmpeg
integration,
where we use ffmpeg as a library instead of calling it as a CLI
program.

One glaring limitation I've encountered with jumpcutter is that
it can't handle larger video files (2 hour 30 minutes +).
Scipy throws an exception complaining the wav is to big.
Since this program doesn't use scipy it doesn't have that issue.

It also appears like jumpcutter is unmaintained.
