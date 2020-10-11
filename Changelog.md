# Change log for cut-the-crap

## Version 2.1.0 - 2020.10.11

+ Automatic cleanup of downloaded files
  This is less annoying and also useful for the hosted version.
+ Add static executable support
  This works for all linux versions, and allows us to drop ubuntu package support.
+ Add default output argument (convenience for debugging)
+ All shell commands run through same function
  This allowed me to improve logging, which better shows what goes on.
+ Add better help docs for input format
  We tell give hints on how to convert to mkv format.

### Cleanup

+ nix: Move upstream patch to separate file and let default.nix depend on that
+ Expose download functions
+ Drop support for Ubuntu
+ Delete segmentsize options which was unused.

### Bug fixes

+ Fix development shell to use nix cabal dependencies
+ Better default options that work with most videos.
+ Set the merge format for youtube dl to always be mkv to prevent issue (fixing the file not found bug)

## Version 2.0.0 - 2020.10.09

- Better install instructions
- `defaut.nix` in project root now links to ffmpeg
- return parse result from `runListenCut`
- Make input file and output file positional
- Add support for parsing uri and downloading automatically

## Version 1.4.2 - 2020.08.31
- add c2hs to build tools so we don't need to modify nixpkgs

## Version 1.4.1 - 2020.08.30
- Add changelog
- Try fix nixpkgs and hackage build.
- Add seperate CI target
- Change overlay to be a copy of nixpkgs

### Known issues

It's unlikely the subtitle generation
will be able to find the model dir:

https://github.com/jappeace/cut-the-crap/issues/45

But this release at least fixes building,
so that editing works again.

## Version 1.4.0 - 2020.08.01
- Add prototype buggy subtitle generation feature

## Version 1.3.0 - 2020.07.24
- Fix more bugs.
- Remove redundant code

## Version 1.2.0 - 2020.07.23
- Fix bugs.
- Better UX

## Version 1.0.0 - 2019.12.23 
- Initial release.
