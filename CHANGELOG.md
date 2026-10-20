# 0.4.2.0 - October 20th, 2026

No API changes from 0.4.0.0. This is the first Hackage release since 0.4.0.0,
so it also includes the 0.4.1.0 changes below.

- Fix `getMany` returning one key's value under another key's name when a
  plain key equals the other's base64 form, such as `"IA=="` and `" "`.
- Requests that carry a value or a number are now encoded into a single
  buffer instead of a chain of appends that copied the value once per piece.
  Encoding a 64KiB `set` takes about 4µs instead of about 57µs.
- Document that a `Version` of `0` skips the CAS check, what `cas`, `replace`,
  `append`, `prepend` and `delete` do for a missing key or a version mismatch,
  and when `increment` and `decrement` create a missing key. Add the missing
  `OverloadedStrings` to the usage examples in `Database.Memcache.Client` and
  the package description, add usage and development instructions to the
  README, and replace the template `SECURITY.md` with a full security policy.

# 0.4.1.0 - October 19th, 2026

Not uploaded to Hackage; these changes first ship there in 0.4.2.0. No API or
behaviour changes from 0.4.0.0.

- Reading a large value no longer allocates memory quadratic in its size. Each
  receive is now capped at 64KiB, so fetching a 32MiB value allocates about
  67MB rather than about 1.7GB.
- Drop the dependency on `errors` (and, through it, `safe` and
  `transformers-compat`), which was only used for a one-line helper.
- Replace the hpack `package.yaml` with a hand-written `memcache.cabal`
  (`cabal-version: 3.0`) that lists the exposed modules explicitly and gives
  each component only the dependencies it uses. Remove `Setup.hs`, which only
  did what `build-type: Simple` does anyway. The license field is now the SPDX
  identifier `BSD-3-Clause`; the license itself is unchanged.
- Declare support for GHC 9.2 to 9.14 in `tested-with`.
- Fix the `parser` benchmark, which no longer compiled, and extend it to cover
  key encoding, more requests, and reading CAS, flags and numbers out of
  responses.
- Expand the package description. Hackage already shows it, as a revision of
  0.4.0.0.
- Document the `InvalidAuthentication` and `KeyTooLong` constructors of
  `ClientError`, and fix typos in the Haddock documentation.
- Greatly expand the test suite. Unit tests against mock servers now cover
  every request encoder and client operation, retries and failover,
  authentication, `parseServerSpec` and ElastiCache discovery, and the
  integration tests also run against memcached in `-Y` auth-file mode and
  against a three-server cluster.
- CI builds and tests with cabal on GHC 9.2 to 9.14 and from the source
  distribution, builds the library against the oldest dependency versions the
  bounds allow, and runs weekly, as well as building with Stack and running
  HLint and weeder.

# 0.4.0.0 - October 19th, 2026

- Replace the deprecated binary protocol with buffered Meta Text requests.
- Add base64 binary-key support, `getMany`, hspec tests, and `Auth`.
- Remove binary protocol and builder dependencies.
- Replace request/response wrapper types with direct Meta Text encoders and `Response`.
- Authentication now uses the server's experimental `-Y authfile` mode
  (introduced in memcached 1.5.15) instead of binary `-S` SASL.
- `encodeKey` now returns a validated `WireKey`; request builders and the
  low-level `keyedOp` APIs accept encoded keys to avoid partial validation and
  repeated encoding.
- Requires memcached 1.6.0 or newer for the meta commands.
- `stats` returns `[(Server, StatResults)]` rather than
  `[(Server, Maybe StatResults)]`; the `Nothing` case was unreachable. Stat
  values containing spaces are no longer truncated, and stats arguments may
  now contain spaces (`stats cachedump 1 100`).
- `Status` loses `ErrInvalidArgs` and `ErrUnknownCommand`, which have no Meta
  Text equivalent and now surface as `ProtocolError`. `ErrValueTooLarge`,
  `ErrOutOfMemory` and `ErrValueNonNumeric` are recovered from the server's
  error text so they keep being reported as `OpError`.
- A command the server rejects (`ERROR`, `CLIENT_ERROR`) or answers with an
  operation status is no longer retried, and no longer counts towards marking
  a server dead.
- `getMany` writes each batch in a single bounded send, so a large key list
  cannot deadlock against the server's replies.
- Response lines are capped at 8KiB, bounding memory use on a desynchronised
  connection.
- Authentication first checks that the server is in auth-file mode and
  refuses to send credentials otherwise, so they are never stored as a plain
  `auth` item. Credentials with a forbidden delimiter, or a server that isn't
  in auth-file mode, raise `ClientError InvalidAuthentication`, and
  credentials the server rejects raise `OpError TextAuthFail`. Neither is
  retried or marks the server dead.
- Empty keys, and keys longer than 250 bytes, are rejected with
  `ClientError KeyTooLong` before anything is sent.
- `append` and `prepend` on a missing key return `Nothing` instead of throwing
  `OpError ErrItemNotStored`.
- `stats` accepts replies made of lines other than `STAT`, such as those from
  `stats reset` and `stats cachedump`.
- `Database.Memcache.Socket` sends and receives on a buffered `Connection`
  with `sendRaw` and `recvResponse`, replacing `send` and `recv`.
- ElastiCache auto discovery no longer holds the server list lock while
  waiting for the next refresh.
- Exception helpers now come from `unliftio`, which is a new dependency.
- Library dependencies declare lower bounds, set to the oldest versions CI
  builds against (GHC 9.2, Stackage LTS 20.26).
- Depend on `data-default` instead of the deprecated `data-default-class`,
  which is now a compatibility shim over `data-default` 0.8.
- Add `Database.Memcache.ElastiCacheClient`, which creates a client from an
  Amazon ElastiCache configuration endpoint and refreshes its node list on an
  interval.
- Add `parseServerSpec`, which reads a `ServerSpec` from a `memcached://` URI.
- `Options` gains `optsServerOptions`, whose `ServerOptions` set the size,
  stripes and keep-alive of each server's connection pool, and
  `optsServerSpecsToServers`, which controls how servers are created.
  `Database.Memcache.Server` exports `newServerDefault` in place of
  `newServer`.

# 0.3.0.2 - March 27th, 2024

- Make the key hashing algorithm configurable by clients -- that is, let
  clients provide a function mapping a key to a server.

# 0.3.0.1 - January 17th, 2021

- No changes, needed a new release to fix a Hackage upload issue.

# 0.3.0.0 - January 17th, 2021

- Bump package dependencies for newer GHC/back/network.
- Update code to work with newer dependencies.

# 0.2.0.1 - November 2nd, 2016

- Fix compatibility with latest `data-default-class`
- Add new ReqRaw type for external clients to implement custom requests. Quite
  a hack right now, so behind a WARNING pragma.

# 0.2.0.0 - May 27th, 2016

- Big design change to reduce code duplication (`Protocol` module gone).
- Remove `Options` type - just fixed configuration for now.
- Design change also allows proper retry handling on operation failure - we
  retry an operation against the same server, but after N consecutive failures,
  we mark the server as dead and don't try using it again until M seconds has
  passed.
- Simplify exception hierarchy - just one type `MemcacheError` now for
  exceptions.
- Remove `defaultOptions` and `defaultServerSpec`, will revisit usefulness.
- Remove many `Typeable` instances.
- Support better testing with a mock Memcached server.
- Fix bug in socket handling - detected EOF properly.
- Greatly improve documentation.
- Use `data-default-class` for defaults of servers and options.

# 0.1.0.1 - February 26th, 2016

- Consistent usage of 'memcached' instead of 'memcache'.
- Document `Database.Memcache.Client`.
- Add inline pragmas in appropriate places.
- Fix bug handling fragmented IP packets (Alfredo Di Napoli).

# 0.1.0.0 - May 18th, 2015

- First proper release (although still lots of rough edges!).
- Filled out `Data.Memcache.Client` to a complete API.
- Integrated cluster and authentication handling.
- Better error and exception handling.
- Fix compilation under GHC 7.10.

# 0.0.1 - May 5th, 2015

- Initial (incomplete) support for a cluster of memcached servers.
- Fixed compilation under GHC 7.4.

# 0.0.0 - August 23rd, 2013

- Initial release. Support for a single server.
