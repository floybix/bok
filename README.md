# Bok

Early stage - experiments only.

> With a triumphant "Pzaarf!", you send your opponent flying over the
> precipice. It writhes maniacally in the air before crashing into the
> dust, and after a while slinks back to join the uneasy throng. You
> allow yourself only a moment to relax and check your limb joints.
> Below, in [Bok
> crater](http://en.wikipedia.org/wiki/Bok_%28Martian_crater%29), more
> challengers await. And with a cosmic ray index this high, evolution is
> in overdrive...

Bok is a set of programming games. You don't play these games
directly, rather you write programs to play them. Your programs
control creatures in a 2D world, with collisions and gravity and such
like. Join games with others locally or over the network, while you
(optionally) watch the fun.

See:

[Games](https://github.com/floybix/bok/wiki/Games)

[Creatures](https://github.com/floybix/bok/wiki/Creatures)

[How to play](https://github.com/floybix/bok/wiki/How-to-play)


## Rationale

For fun of course. But also: to test evolution and intelligence
algorithms in a rich-enough problem domain, and compare them to
human-designed solutions.


## Running bouts

Get [Leiningen](http://leiningen.org/) first.

You can run some in-process demos:

```
cd bok/runner
lein run -m org.nfrac.bok.tests.bipoid sumo
lein run -m org.nfrac.bok.tests.humanoid vortex-maze
lein run -m org.nfrac.bok.tests.wormoid climbly-altitude
lein run -m org.nfrac.bok.tests.bipoid bowl-energy-race
lein run -m org.nfrac.bok.tests.bipoid cavern-hunt
```

Or try running player servers communicating over TCP:

```
cd bok/players/clojure
lein run -m org.nfrac.bok.examples.bipoid1 5555
lein run -m org.nfrac.bok.examples.humanoid1 5556
cd bok/runner
lein run -m org.nfrac.bok.visual-runner sumo tcp://localhost:5555 tcp://localhost:5556
```

Or use the Clojure REPL: see
[REPL session](https://github.com/floybix/bok/wiki/REPL-session)


## License

Copyright © 2015 Felix Andrews

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
