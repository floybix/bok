# Hatto

A programming game. Program behaviour for a creature in a 2D world to
wrestle its opponent off the stage.

Very early stage - experiments only.


## Usage

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

Then you can run demos:

<pre>
lein run -m org.nfrac.hatto.tests.nin
lein run -m org.nfrac.hatto.tests.legge
lein run -m org.nfrac.hatto.tests.hatto
</pre>


## License

Copyright © 2015 Felix Andrews

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
