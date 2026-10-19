# memcache: Haskell Memcached Client

[![Hackage](https://img.shields.io/hackage/v/memcache.svg?style=flat)](https://hackage.haskell.org/package/memcache)
[![BSD3 License](https://img.shields.io/badge/license-BSD3-brightgreen.svg?style=flat)][tl;dr Legal: BSD3]
[![CI](https://github.com/dterei/memcache-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/dterei/memcache-hs/actions/workflows/ci.yml)

[tl;dr Legal: BSD3]: https://tldrlegal.com/license/bsd-3-clause-license-(revised) "BSD3 License"

A client library for a memcached cluster.

It supports the Meta Text memcached protocol and username/password
authentication. Keys containing whitespace or control bytes are sent using its
base64 binary-key mode. The encoded key must still fit within 250 bytes. It
supports connecting to a single, or a cluster of memcached servers. When
connecting to a cluster, consistent hashing is used for routing requests to
the appropriate server.

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

## Usage

Add `memcache` to your package's `build-depends`. Everything most programs need
is exported from `Database.Memcache.Client`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Database.Memcache.Client as M

main :: IO ()
main = do
  -- Connect to memcached on 127.0.0.1:11211 with the default options.
  mc <- M.newClient [M.def] M.def

  -- Store a value with flags 0 that never expires, then read it back along
  -- with its flags and CAS version.
  _ <- M.set mc "greeting" "hello" 0 0
  hit <- M.get mc "greeting"
  case hit of
    Nothing -> putStrLn "miss"
    Just (value, _flags, version) -> do
      print value
      -- Only overwrite the value if it hasn't changed since we read it.
      updated <- M.cas mc "greeting" "hello again" 0 0 version
      putStrLn (maybe "changed by someone else" (const "updated") updated)

  -- Create the counter as 1 if it is missing, otherwise add 1 to it.
  visits <- M.increment mc "visits" 1 1 0 0
  print (fst <$> visits)

  M.quit mc
```

Keys and values are strict `ByteString`s. Misses are returned as `Nothing`,
while unexpected failures are thrown as a `MemcacheError` or a network
`IOException`. Operations that take a CAS `Version` skip the check when it is
`0`. `increment` and `decrement` create a missing key with the given initial
value, unless the expiration is `maxBound`. The
[`Database.Memcache.Client`](https://hackage.haskell.org/package/memcache/docs/Database-Memcache-Client.html)
documentation covers every operation, along with the timeouts, retries and
failover configured through `Options`.

### Servers and authentication

`newClient` takes one `ServerSpec` per server, and routes each key to one of
them. Credentials are set per server:

```haskell
  mc <- M.newClient
    [ M.ServerSpec "cache1.internal" "11211" (M.Auth "user" "secret")
    , M.ServerSpec "cache2.internal" "11211" (M.Auth "user" "secret")
    ]
    M.def
```

`parseServerSpec`, from `Database.Memcache.Types`, reads a server from a
`memcached://[user:password@]host[:port]` URI instead. The host defaults to
`127.0.0.1` and the port to `11211`:

```haskell
import Database.Memcache.Types (parseServerSpec)

  spec <- either fail pure (parseServerSpec "memcached://user:secret@cache1.internal:11211")
  mc <- M.newClient [spec] M.def
```

The server must be started with `-Y <authfile>`, where the auth file holds one
`username:password` line per user. This library connects without TLS, so
credentials and all data are sent unencrypted; only rely on them within a
trusted network.

### Amazon ElastiCache

`Database.Memcache.ElastiCacheClient` creates a client from an ElastiCache
configuration endpoint (the address containing `.cfg`). It discovers the
cluster's nodes, then refreshes the node list in a background thread on the
given interval, in microseconds as for `threadDelay`:

```haskell
import qualified Database.Memcache.ElastiCacheClient as EC

  endpoint <- either fail pure $
    EC.parseConfigurationEndpoint "memcached://mycluster.abc123.cfg.use1.cache.amazonaws.com:11211"
  mc <- EC.newClient M.def (60 * 1000000) endpoint
```

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

We're relying on `Data.Pool` for thread safety right now. Each server has its
own pool, and grabbing a connection from it (`withResource`) blocks other
requests using that connection until the operation completes. `getMany` is the
exception: it pipelines quiet Meta Text requests over one pooled connection.

By default each server's pool holds a single connection (`soNumResources = 1`),
so concurrent operations against the same server wait for each other. Raising
`soNumResources` in the `ServerOptions` set through `optsServerOptions` allows
concurrent operations without requiring a separate pool implementation.

Either way, a pool is fine for now.

## Other clients

- [C: libmemcached](http://libmemcached.org/libMemcached.html)
- [Java: SpyMemcached](http://code.google.com/p/spymemcached/)
- [Ruby: Dalli](https://github.com/mperham/dalli)

## Get involved!

We are happy to receive bug reports, fixes, documentation enhancements, and
other improvements.

Please report bugs via the
[github issue tracker](https://github.com/dterei/memcache-hs/issues), and
security issues as described in [SECURITY.md](SECURITY.md).

Master [git repository](https://github.com/dterei/memcache-hs):

- `git clone https://github.com/dterei/memcache-hs.git`

## Development

The package builds with [Stack](https://docs.haskellstack.org/) or
[cabal](https://www.haskell.org/cabal/), on GHC 9.2 or newer:

```sh
stack build
stack test

cabal build
cabal test all
```

`stack.yaml` uses Stackage LTS 22.14 (GHC 9.6). The oldest and newest snapshots
CI tests against have their own files, for example
`stack --stack-yaml stack-lts-20.26.yaml test` for GHC 9.2.

There are two test suites:

- `spec` holds the unit and property tests, which run against an in-process
  mock server and need nothing installed.
- `integration` starts its own `memcached` servers on free local ports and
  tests against them, so it needs memcached 1.6 or newer on your `PATH`.
  Without it, the suite reports a single pending test instead of failing.
  memcached refuses to run as root without `-u`, so run the suite as an
  ordinary user.

CI lints with [HLint](https://github.com/ndmitchell/hlint) 3.10, so please
check that `hlint .` prints `No hints` before opening a pull request. Add a
line to the `Unreleased` section of [CHANGELOG.md](CHANGELOG.md) for any
user-visible change.

CI runs both build tools on every pull request. The `CI` workflow builds and
tests with Stack against each `stack*.yaml` snapshot, and runs HLint and
weeder. The `Cabal` workflow builds and tests with cabal on GHC 9.2 to 9.14,
builds the library against the oldest dependency versions its bounds allow,
and builds and tests the source distribution; it also runs every Monday, so a
new dependency release that breaks the build shows up without a push. Both
install memcached, so the integration suite runs in full.

## Authors

This library is written and maintained by David Terei (<code@davidterei.com>).

Contributions have been made by the following great people:

- Alfredo Di Napoli (<alfredo.dinapoli@gmail.com>)
- Amit Levy
- Chris Martin
- drlkf
- Joel McCracken
- Steven Leiva
