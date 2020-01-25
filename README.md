# erdos.yield

Python's [generator/yield](https://wiki.python.org/moin/Generators) feature in Clojure and ClojureScript to generate lazy sequences.

How is it different from [clj-generators](https://github.com/aengelberg/clj-generators) and [clj-yield](https://github.com/jpalmucci/clj-yield)? It is much simpler with lower overhead, because it does not depend on core.async or threading in the background. It does simple term rewriting to change imperative style code to functional sequence compositions.

[![Clojars Project](https://img.shields.io/clojars/v/io.github.erdos/erdos.yield.svg)](https://clojars.org/io.github.erdos/erdos.yield)
[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/erdos/erdos.yield/issues)
[![HitCount](http://hits.dwyl.io/erdos/erdos.yield.svg)](http://hits.dwyl.io/erdos/erdos.yield)
[![EPL 2.0](https://img.shields.io/badge/License-EPL%202.0-red.svg)](https://www.eclipse.org/legal/epl-2.0/)


## Usage

Use the `(gen-seq)` macro to create a lazy sequence. Invocations of `(yield)` in the macro sets the next return value of the sequence.

For example, `(gen-seq (yield 1) (yield 2))` will return `(1 2)`. A more complex example:

```Clojure
(gen-seq
 (dotimes [i 3]
   (yield i))
 (yield "Hello")
 (doseq [c "Greetings"]
   (yield c))
 (loop [i 3]
   (when (pos? i)
     (yield i)
     (recur (dec i))))
 (yield "End."))
```

Outputs:

```Clojure
(0 1 2 "Hello" \G \r \e \e \t \i \n \g \s \3 \2 \1 "End.")
```

Use `(yield-all xs)` instead of `(doseq [x xs] (yield x))` to set multiple elements in the sequence.


## License

Copyright © 2020 Janos Erdos

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
