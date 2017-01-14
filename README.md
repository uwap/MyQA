# MyQA

A web service to annonymously ask questions and publically answer them. Inspired by ask.fm and curiouscat.me.
You can host MyQA yourself. Simply follow the How To Install guide.

## How to install

Install PostgreSQL 9.1 or above. Create a new user and a new database. Grant the user all access on that database
and set the user, database and password in the config file. Install cabal and run

```
cabal install --dependencies-only && cabal build
```

You can then run the program with `cabal run`.

To finally install the program run `cabal install` and then add ~/.cabal/bin/ to your $PATH variable.
