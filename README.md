# memcache: Haskell Memcache Client

This library provides a client interface to a Memcache cluster. It is
aimed at full binary protocol support, ease of use and speed.

## Licensing

This library is BSD-licensed.

## Tools

This library also includes a few tools for manipulating and
experimenting with memcached servers.

* `OpGen` -- A load generator for memcached. Doesn't collect timing
  statistics, other tools like
  [mutilate](https://github.com/leverich/mutilate) already do that
  very well. This tool is useful in conjunction with mutilate.
* `Loader` -- A tool to load random data of a certain size into a
  memcache server. Useful for priming a server for testing.

## To do

* Timeouts
* Ring / CHORD support
* Thread-safe?

* mutli(-get)

* Connection drop / reconnect resiliance?
* Asynchronous support?
* Connection pooling?
* Tweaking? (e.g., drop in hash algorithm?, timeout, max connection
  retries...)
* Max value validation...?

* Optimizations? http://code.google.com/p/spymemcached/wiki/Optimizations

* Typeclass for serialization?
* Monad / Typeclass for memcache?

* UDP
* ASCII

## Other clients

* [C: libmemcached](http://libmemcached.org/libMemcached.html)
* [Java: SpyMemcached](http://code.google.com/p/spymemcached/)
* [Ruby: Dalli](https://github.com/mperham/dalli)

## Get involved!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/dterei/mc-hs/issues).

Master [git repository](http://github.com/dterei/mc-hs):

* `git clone git://github.com/dterei/mc-hs.git`

## Authors

This library is written and maintained by David Terei,
<code@davidterei.com>.

