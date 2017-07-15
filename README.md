# vidtracker

blog post about this [here](http://qiita.com/kimagure/items/b576b5bfe370180599f8)

a simple application written with [Hyper](https://github.com/owickstrom/hyper) to keep track of videos watched and make opening them easier

features a [Halogen](https://github.com/slamdata/purescript-halogen) front end. full stack purescript!!

uses DB file "filetracker" `CREATE TABLE watched (path varchar(20) primary key unique, created datetime);` in your target `FILETRACKER_DIR` directory

[video example](https://twitter.com/jusrin00/status/843025971234177024)

Screenshot:

![](http://i.imgur.com/SG6vSil.png)
