# vidtracker

[![Build Status](https://travis-ci.com/justinwoo/vidtracker.svg?branch=master)](https://travis-ci.com/justinwoo/vidtracker)

blog post about this [here](http://qiita.com/kimagure/items/b576b5bfe370180599f8)

more up-to-date blog post about the routing part of this [here](https://qiita.com/kimagure/items/bb9bd3e4ffe1bba4c214)

even newer, more general post here: <https://github.com/justinwoo/my-blog-posts#opting-in-to-better-types-and-guarantees-in-purescript>

a simple "full-stack" purescript application to keep track of videos watched and make opening them easy

[video example](https://twitter.com/jusrin00/status/843025971234177024)

## Requirements:

* sqlite3
* on Windows: rust-vlc-finder: https://github.com/justinwoo/rust-vlc-finder
* a config.ini that contains...
    * location of your videos
    * the executable you will use

Like so:
```
[vidtracker]
dir=/home/user/Videos
exe=vlc
```

Screenshot:

![](https://i.imgur.com/yqeKoq5.png)
