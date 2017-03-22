# vidtracker

like [filetracker](https://github.com/justinwoo/filetracker), but written with [Hyper](https://github.com/owickstrom/hyper)

features a [Halogen](https://github.com/slamdata/purescript-halogen) front end. full stack purescript!!

uses DB file "filetracker" `CREATE TABLE watched (path varchar(20) primary key unique, created datetime);` in your target `FILETRACKER_DIR` directory