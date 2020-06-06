# vidtracker -- abandoned

I use a newer version of this tool now. While you might refer to this repo to try to learn about some various things, you should know that I do not use this repo as-is anymore, and have not for a long time.

-----

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

![notwatched](https://user-images.githubusercontent.com/2396926/54077857-05846a80-42c7-11e9-9aff-944790cc0600.png)
![watched](https://user-images.githubusercontent.com/2396926/54077858-05846a80-42c7-11e9-8c24-16dc5d5b5df4.png)
