# populate

A simple command line app to populate music files.

## What exactly is this?

This is an app that I wrote because it was useful to me, but I hope
it can be useful to other people to. The idea is to take a file
specifying a library of music (song name, artist, where to get it etc)
the app fetches the song and splits it based on provided time stamps if necessary.

For example, a file like:
```toml
[[source]]
  name = "Gattsu"
  artist = "Susume Hirasawa"
  path = "Susume Hirasawa/"
  url = "https://www.youtube.com/watch?v=_isSnrC2__A"
  timestamps = ["0:00", "1:00"]
  namestamps = ["first", "second"]
```
specifies how to get an album named "Gattsu" by an artist
named "Susume Hirasawa", by fetching it from a url and then unpacking
it into the songs that compose it in a certain directory.

The app takes a bunch of sources like these and downloads and unpacks them all.

## Building
This program relies on having the cli applications `youtube-dl` and `ffmpeg` available.
`taglib_c` is also needed for adding metadata to the audio files.
On my system (ubuntu) acquiring these depencies looks like this:
```
sudo apt-get install ffmpeg
sudo apt-get install youtube-dl
sudo apt-get install libtagc0-dev
```
Feel free to submit corresponding instructions for your platform :)

With stack, building and installing the program looks like this:
```
stack install
```

## Usage
```
Usage: populate FILE [-w|--overwrite]
  Populate based on the songs in FILE

Available options:
  -w,--overwrite           Whether to overwrite cached files
  -h,--help                Show this help text
```

For examples on what this file should look like, check out the `examples` directory.
