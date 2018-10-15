# AcidView 
Helper utility that prints contents of Acid DB to the terminal or a file.

One can view the state of the database `n` events ago (up to the last checkpoint).
```
Usage: acid-view [--dbpath DBPath] [-p|--position Steps]
                 [-s|--show TypeOfOutput] [-f|--file FileToSave]
```

Example usage:
```
acidView --dbpath /home/username/DBPath -p 2 -s addr
``` 
   Prints to stdout addresses that were in the DB 2 events ago.
```
acidView -f DBDump.txt
``` 
Saves full contents of the DB (from default path) to `DBDump.txt`.

One can also run `acidView -h` to get help. 