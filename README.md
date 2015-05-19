# memcache: Haskell Memcache Client

[![Hackage version](https://img.shields.io/hackage/v/memcache.svg?style=flat)](https://hackage.haskell.org/package/memcache) [![Build Status](https://img.shields.io/travis/dterei/memcache-hs.svg?style=flat)](https://travis-ci.org/dterei/memcache-hs)

A client library for a memcached cluster.

It supports the binary memcached protocol and SASL authentication. No support
for the ASCII protocol is provided. It supports connecting to a single, or a
cluster of memcached servers. When connecting to a cluser, consistent hashing
is used for routing requests to the appropriate server.

Complete coverage of the memcached protocol is provided except for multi-get
and other pipelined operations.

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

## Architecture Notes

We're relying on `Data.Pool` for thread safety right now, which is
fine but is a blocking API in that when we grab a socket
(`withResource`) we are blocking any other requests being sent over
that connection until we get a response. That is, we can't pipeline.

Now, use of multiple connections through the pool abstraction is an
easy way to solve this and perhaps the right approach. But, could also
implement own pool abstraction that allowed pipelining. This wouldn't
be a pool abstraction so much as just round-robbining over multiple
connections for performance.

Either way, a pool is fine for now.

## Other clients

* [C: libmemcached](http://libmemcached.org/libMemcached.html)
* [Java: SpyMemcached](http://code.google.com/p/spymemcached/)
* [Ruby: Dalli](https://github.com/mperham/dalli)

## Get involved!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/dterei/memcache-hs/issues).

Master [git repository](http://github.com/dterei/memcache-hs):

* `git clone https://github.com/dterei/memcache-hs.git`

## Authors

This library is written and maintained by David Terei,
<code@davidterei.com>.

