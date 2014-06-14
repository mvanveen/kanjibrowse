kanjibrowse
=========

Display the structure of kanji.

## Installation

### Download & Extract the KanjiVG Database
```
wget -P db https://github.com/KanjiVG/kanjivg/releases/download/r20130901/kanjivg-20130901-all.zip
unzip db/kanjivg-20130901-all.zip -d db
```

### Get GHC & Cabal
On Ubuntu, run:
```
sudo apt-get install haskell-platform
cabal update
```

### Create Cabal Sandbox
```
cabal sandbox init
```

### Install Dependencies
```
cd /path/to/kanjibrowse/repo
cabal install --only-dependencies
```

### Build
```
cabal build
```

### Start the Server
```
./dist/build/server/server
```

### Enjoy
Point your browser at http://localhost:3000/働
