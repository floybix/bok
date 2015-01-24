# Hatto ハット"

Early stage - experiments only.

Hatto are programming games. You don't play these games directly.
Instead you write a program to control a creature. Your program is the
player, and you tell it how to play. The games take place in a 2D
world, with collisions and gravity and such like.

See:

[Games](https://github.com/floybix/hatto/wiki/Games)

[Creatures](https://github.com/floybix/hatto/wiki/Creatures)

[How to play](https://github.com/floybix/hatto/wiki/How-to-play)


## Running bouts

Get [Leiningen](http://leiningen.org/) first.

You can run some in-process demos:

```
cd hatto/runner
lein run -m org.nfrac.hatto.tests.legsoid sumo
lein run -m org.nfrac.hatto.tests.humanoid vortex-maze
lein run -m org.nfrac.hatto.tests.wormoid altitude
lein run -m org.nfrac.hatto.tests.legsoid energy-race
lein run -m org.nfrac.hatto.tests.legsoid hunt
```

Or try running player servers communicating over TCP:

```
cd hatto/players/clojure
lein run -m org.nfrac.hatto.examples.legsoid1 5555
lein run -m org.nfrac.hatto.examples.legsoid1 5556
cd hatto/runner
lein run -m org.nfrac.hatto.visual-runner sumo tcp://localhost:5555 tcp://localhost:5556
```

Or use the Clojure REPL: see
[REPL session](https://github.com/floybix/hatto/wiki/REPL-session)


## License

Copyright © 2015 Felix Andrews

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
