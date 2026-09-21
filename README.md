# memcache: Haskell Memcached Client

[![Hackage](https://img.shields.io/hackage/v/memcache.svg?style=flat)](https://hackage.haskell.org/package/memcache)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/memcache.svg?style=flat)](http://packdeps.haskellers.com/reverse/memcache)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg?style=flat)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/dterei/memcache-hs.svg?style=flat)](https://travis-ci.org/dterei/memcache-hs)
[![Gitter](https://badges.gitter.im/dterei/memcache-hs.svg)](https://gitter.im/dterei/memcache-hs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

[tl;dr Legal: BSD3]: https://tldrlegal.com/license/bsd-3-clause-license-(revised) "BSD3 License"

A client library for a memcached cluster.

It supports the Meta Text memcached protocol and username/password
authentication. Keys containing whitespace or control bytes, or exceeding the
protocol's key-length limit, are sent using its base64 binary-key mode. The
encoded key must still fit within 250 bytes. It supports connecting to a
single, or a cluster of memcached servers. When connecting to a cluster,
consistent hashing is used for routing requests to the appropriate server.

**This library requires memcached 1.6.0 or newer**, the first release with the
meta commands. Against older servers every operation is rejected with `ERROR`.

Authentication uses a memcached auth file with `-Y authfile`, an experimental
ASCII-protocol auth mode added in memcached 1.5.15; the old binary protocol
`-S` SASL mode is not supported. Enabling `-Y` also disables binary and UDP
protocols on the server. `getMany` uses quiet misses, so a server connection
failure can be indistinguishable from a cache miss.

The client operations, including `getMany`, use direct Meta Text encoders and
the `Response` type; the former request and response wrapper types are no longer
provided.

## Licensing

This library is BSD-licensed.

## Tools

This library also includes a few tools for manipulating and experimenting with
memcached servers.

- `OpGen` -- A load generator for memcached. Doesn't collect timing statistics,
  other tools like [mutilate](https://github.com/leverich/mutilate) already do
  that very well. This tool is useful in conjunction with mutilate.
- `Loader` -- A tool to load random data of a certain size into a memcached
  server. Useful for priming a server for testing.

## Architecture Notes

We're relying on `Data.Pool` for thread safety right now. Grabbing a connection
from the pool (`withResource`) blocks other requests using that connection
until the operation completes. `getMany` is the exception: it pipelines quiet
Meta Text requests over one pooled connection.

Multiple connections through the pool abstraction allow concurrent operations
and provide a simple performance path without requiring a separate pool
implementation.

Either way, a pool is fine for now.

## Other clients

- [C: libmemcached](http://libmemcached.org/libMemcached.html)
- [Java: SpyMemcached](http://code.google.com/p/spymemcached/)
- [Ruby: Dalli](https://github.com/mperham/dalli)

## Get involved!

We are happy to receive bug reports, fixes, documentation enhancements, and
other improvements.

Please report bugs via the
[github issue tracker](http://github.com/dterei/memcache-hs/issues).

Master [git repository](http://github.com/dterei/memcache-hs):

- `git clone https://github.com/dterei/memcache-hs.git`

## Authors

This library is written and maintained by David Terei (<code@davidterei.com>).

Contributions have been made by the following great people:

- Alfredo Di Napoli (<alfredo.dinapoli@gmail.com>)
- Amit Levy
- Steven Leiva
