Readme
======
hs-arena is an experimental and simple Zelda-like 2D hack & slay game written in
haskell.  You have to defeat 4 different types of monsters in each round. After
each round the monster become a bit stronger (= have more HPs) and/or increase
in numbers.

Installation
------------

It has the following dependencies:

* SDL (0.6.x)
* SDL-image (0.6.x)
* SDL-mixer (0.6.x)
* PSQueue (1.1)
* HaXml (1.20.x)

It was tested on Linux (Arch Linux) and Mac OSX with ghc 6.12

Controls
--------

You must use the following keys to control the game:

* Arrow-Keys: Move left/up/down/right
* Space-Bar: Attack
* z: Switch to previous weapon
* x: Switch to next weapon
* q: Exit the game (during play)
