# Brolib

The code in this repository implements a purely functoinal Rope data structure, using the balancing scheme (with slight modifications to suit Ropes) from [Ralf Hinze's 1-2 Brother Trees paper](https://www.cs.ox.ac.uk/ralf.hinze/publications/Brother12.pdf).

The file /lib/tiny_rope.ml is a standard UTF-8 rope with the core insert, delete, substring and to string operations. There are also implementations of some functions from the standard library's `String` module.

The file at /lib/utf8_rope.ml is exactly the same, but it allows line queries as well (enabled by tracking line break information in an array). It is missing the implementation of functions from the `String` module.

In the /bin/ folder, all files except dune, utils.ml and main.ml are arrays generated from the sequential edit traces at [the linked repository](https://github.com/josephg/editing-traces). (The files utils.ml and main.ml just apply the edit traces for benchmarking purposes.)

Both of the mentioned files under the /lib/ directory are under the permissive 0BSD license. Feel free to use them in any way you like.

There are [F#](https://github.com/hummy123/brolib-fs) and [Standard ML](https://github.com/hummy123/brolib-sml) ports of tiny_rope.ml which are also under 0BSD.
