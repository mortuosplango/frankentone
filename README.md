franken[~]tone
===========

Experimental library for live coding in clojure. Tries to play nice
with overtone. Steals a lot of ideas from other live coding
environments.

## Usage

Clone this repository.

"lein run" brings up the editor.

There's a tutorial in "Help->Getting Started".

## Credits

Thanks to the power of open source, large parts of this system are
heavily inspired by or direct ports from other languages/systems.

### Patterns

Based on Alex McLean's [Tidal](http://yaxu.org/tidal/)

### DSP, Instruments, UGens

A lot of the DSP architecture, the Instrument system and a lot of
UGens started as ports of Andrew Sorensen et. al.'s
[Extempore](https://github.com/digego/extempore)

### UGens, ...

As I used a lot of
[SuperCollider](http://supercollider.sourceforge.net/) in my time, it
is a major influence. Also I port[ed] some UGens.

### Infrastructure

[Overtone](https://github.com/overtone/) is used in this project
because it already provides a lot of useful infrastructure for music
programming on Clojure.


*Thank you all!*

## License

Copyright © 2013 Holger Ballweg <mortuos.plango@gmail.com>

Distributed under the Eclipse Public License, the same as Clojure.
