# Hatto ハット"

Very early stage - experiments only.

Hatto is a programming game. That means you don't play the game
directly. Instead you make a program to play it. Your program is the
player, and you tell it how to play.

The game takes place in a 2D world (with physics and gravity). The aim
is to push your opponent off the arena, or at least avoid falling off
until your opponent falls first. It's like a wrestling match.


See:

[Creatures](https://github.com/floybix/hatto/wiki/Creatures)

[How to play](https://github.com/floybix/hatto/wiki/How-to-play)



## Installation

Get [Leiningen](http://leiningen.org/) first.

Clone [cljbox2d](http://github.com/floybix/cljbox2d/),
and install it to your local Maven repository (~/.m2):

```
cd cljbox2d
lein install
```

also the cljbox2d testbed project:

```
cd cljbox2d/testbed
lein install
```

Then you can run basic in-process demos:

```
cd hatto/runner
lein run -m org.nfrac.hatto.tests.alexis
lein run -m org.nfrac.hatto.tests.hugh
lein run -m org.nfrac.hatto.tests.nick
```

Or try running players over the wire:

```
cd hatto/players/clojure
lein run -m org.nfrac.hatto.examples.alexis1 5555 &
lein run -m org.nfrac.hatto.examples.alexis1 5556 &
cd hatto/runner
lein run -m org.nfrac.hatto.visual-runner tcp://localhost:5555 tcp://localhost:5556 sandbox
```

Or use the Clojure REPL: see
[REPL session](https://github.com/floybix/hatto/wiki/REPL-session)


## License

Copyright © 2015 Felix Andrews

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
