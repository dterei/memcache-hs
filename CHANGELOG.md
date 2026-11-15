# 0.5.0.0 - November 18th, 2026

This release changes how a cluster routes keys to its servers, so upgrading
moves keys to other servers, once; see "Upgrading from 0.4" in the README. It
also changes what `optsServerRetries` counts, the types of two lower-level
functions, and some return values and exceptions, so read the breaking
changes first. Two security fixes stop memcached from running the bytes of a
value being stored as commands, and memcached 1.6.10 or later is now
required; see Security.

## Security

Both fixes are also in 0.4.2.1.

- `set`, `add`, `replace`, `cas`, `append`, `prepend` and `setMany` reject a
  value larger than 1GiB, memcached's largest possible item size, with
  `OpError ErrValueTooLarge` before sending anything. A value of about 2GiB
  or more (longer than 2147483645 bytes) previously made memcached run the
  value's bytes as commands, so storing untrusted data could run commands on
  the server, and a value over 1GiB could time out, be sent again and mark
  the server dead. Such a value no longer reaches the server, so it no
  longer makes memcached delete the key's existing value, as memcached does
  whenever it rejects a store as too large; a value within 1GiB but over the
  server's own limit (`-I`, 1MiB by default) still does. Every operation
  that sends a value now evaluates it first, before its timeout starts: an
  exception it throws is no longer retried and no longer marks the server
  dead, and time spent computing it no longer counts towards
  `optsServerTimeout`. `setMany` evaluates every value before it sends
  anything, so a list of lazily computed values is all in memory at once,
  and one value over the limit stores none of the pairs. The request
  encoders in `Database.Memcache.Socket` still encode a value of any length;
  don't send one longer than 1GiB.
- memcached 1.6.10 or later is now required (previously documented as
  1.6.0). memcached before 1.6.10 rejects the meta set command this library
  stores values with, without reading the value that follows it, and then
  runs the value's bytes as commands, so storing untrusted data could run
  commands on the server. Each new connection now asks the server for its
  version before it is used: until that check passes, the only requests sent
  are the authentication exchange, when credentials are set, and `version`.
  A server older than 1.6.10, or any other reply that is neither an error
  nor a readable version, is refused with `ProtocolError (UnknownOp ...)`,
  whose message quotes the reply; the connection is closed, and the refusal
  is not retried and doesn't mark the server dead. An error reply, such as
  `CLIENT_ERROR unauthenticated`, throws what it throws for any request, and
  so does a reply that can't be parsed, such as an empty line or an overlong
  one, which counts against the server. This covers connections over TCP and
  Unix sockets, and ElastiCache discovery, which now fails for a cluster
  running a memcached older than 1.6.10. Through a proxy, the check sees
  only the proxy's version, so every memcached behind it must be 1.6.10 or
  later too. Each new connection takes one more round trip, which counts
  towards `optsServerTimeout`. Connections made directly with
  `Database.Memcache.Socket.newConnection` are not checked; send
  `storeRequest`, `setManyRequest` and `modifyRequest` only to 1.6.10 or
  later.

## Breaking changes

- memcached 1.6.10 or later is now required (see Security): a new
  connection to an older server is refused, so every operation that needs
  one fails with `ProtocolError (UnknownOp ...)`.
- Clusters route keys with ketama consistent hashing, compatible with
  libmemcached (see New): `ketamaServerForKey` is the new default
  `optsGetServerForKey`. 0.4's router sent keys to no more than two servers
  at a time, and the server it chose for a key could change with the
  `hashable` version. With n servers, upgrading moves all but about one key
  in n to a different server, once: about half the keys with 2 servers, 70%
  with 3 and 88% with 8. A single server is unaffected. While old and new
  versions run side by side, as in a rolling deploy, they read and write a
  moved key on different servers; "Upgrading from 0.4", in the README and in
  `Database.Memcache.Cluster`, gives two ways to switch. One of them uses
  `legacyServerForKey`, 0.4's router, which `Database.Memcache.Cluster` now
  exports, deprecated, for one release; it will be removed in the next major
  version. A key whose server is marked failed now goes to the next live
  server on the ring, and other keys stay where they are.
- With keys spread over every server, `getMany` sends each server its keys
  in batches of up to 256, one round trip per batch and one server after
  another, so it can take longer than before with three or more servers. It
  still returns its results grouped by server, so their order changes too.
- Servers keep the order they are configured or discovered in, instead of
  being sorted by `sid`: `getServers` and `stats` list them in that order,
  `flush` goes through them in it, and `version` asks the first live one.
- `optsServerRetries` now counts retries, as its documentation said, rather
  than attempts: an operation is tried up to `optsServerRetries + 1` times,
  and once for 0 or a negative value. So a value of 1 or more that you set
  yourself makes one more attempt than in 0.4; set it one lower to keep the
  old number of attempts. `minBound`, which used to retry practically
  without end, now gives one attempt. The default drops from 2 to 1, so by
  default an operation is still tried twice.
- `increment`, `decrement`, `append`, `prepend`, `add`, `delete`, and `cas`
  and `replace` with a version other than 0 are no longer sent again once
  their request may have reached the server, so they can't take effect twice
  or report failure because their first request succeeded. Once the client
  has begun to send the request, a failure such as a timeout, a network
  error, a dropped connection, a reply that can't be read or a
  `SERVER_ERROR` whose text names no status is thrown as it is, whatever
  retries are left, and marks the server dead. A failure before sending,
  such as a refused connection or a timeout while waiting for a pooled
  connection, is still retried. Before sending one of these requests on a
  pooled connection, the client checks, without waiting, that nothing is
  left to read on it and that the server hasn't closed or reset it, as
  memcached closes idle connections when it restarts or after
  `-o idle_timeout`. If the check fails, the client retries the request on a
  new connection when retries are left; on Windows, the check only sees what
  has already been read, so it can't see a closed connection. Other
  operations are still retried after any failure that counts against the
  server, so the server may carry out their request twice.
- When a server's host name resolves to several addresses, the client now
  tries each in turn, not just the first (see Fixes). So when the first
  address refuses and a later one doesn't answer, an attempt throws
  `ClientError Timeout` once its `optsServerTimeout` runs out, instead of
  the first address's `IOException` at once. If the addresses are separate
  memcached instances, give each its own `ServerSpec`: while the first
  refuses connections, new connections go to the next one that accepts, and
  so to another cache.
- `append` and `prepend` return `Nothing` when a non-zero version doesn't
  match, as `cas` and `replace` do, instead of throwing
  `OpError ErrKeyExists`.
- `flush` stops at the first server whose reply it rejects (see Fixes),
  leaving the servers after it unflushed, where 0.4 checked such a reply
  only after sending `flush_all` to every server.
- memcached's capitalised out-of-memory errors are thrown as
  `OpError ErrOutOfMemory`, like the lowercase ones, instead of
  `ProtocolError ServerError` (see Fixes), so the request is no longer sent
  again, and they don't count towards marking the server dead. Code that
  matched `ServerError` for them should match `ErrOutOfMemory` instead.
- Numbers in replies (value lengths, CAS versions, flags and counter values)
  must be plain decimal digits that fit their type, as memcached writes them
  (see Fixes), where 0.4 also read forms such as `(5)` and `0x10`, and
  wrapped numbers around to fit. So from a server or proxy that writes them
  otherwise, a value length now throws `BadLength`, and a CAS version, flags
  or counter value reads as 0. A `VA` reply without a value length, which
  used to read as a hit with an empty value, throws `BadLength` too.
- `displayException` for a `MemcacheError` gives a one-line message, such as
  `memcached operation failed: key not found`, where it used to give the
  same text as `show`, such as `OpError ErrKeyNotFound`; GHC 9.10 and later
  also print it for an uncaught exception. Any detail, such as text from the
  server, is cut to its first 200 characters, with backslashes and
  characters outside printable ASCII escaped. `show` is unchanged, so code
  that parses `displayException`'s text should match on the constructors, or
  use `show`, instead.
- `ElastiCacheClient.newClient` throws `ClientError Timeout` unless its
  first discovery, connecting included, finishes within `optsServerTimeout`.
  It had no time limit, so with the default of 750ms, a slow DNS lookup or
  connection can now make it fail. An `ERROR`, `CLIENT_ERROR` or
  `SERVER_ERROR` reply to the request for the cluster configuration, such as
  the `ERROR` that plain memcached gives, throws a `StringException` from
  `UnliftIO.Exception` quoting the line, escaped and cut to 200 characters,
  where `newClient` used to block forever, or throw
  `ProtocolError UnexpectedEOF` if the endpoint then closed the connection.
  Discovery now calls `optsServerSpecsToServers` with one node at a time.
- `stats` throws `BadLength` for a server's reply of more than 262,144
  entries, or of more than 16MiB counting each line as it was sent, such as
  the reply to `stats conns` on a server with more than about 65,500 open
  connections. `stats` retries it, as it does other server failures, and it
  counts towards marking the server dead. `recvRawUntil` throws `BadLength`
  once it has read more than 1MiB without its delimiter, so ElastiCache
  discovery does too for a version line or configuration reply that long. No
  option raises these limits.
- `parseServerSpec`, and so `parseConfigurationEndpoint`, percent-decodes
  the username and password of a `memcached://` URI to the bytes they
  encode, so `memcached://user:p%40ss@cache.example` has the password
  `p@ss`, and returns `Left` if the decoded credentials fail
  `validateCredentials`. `validateCredentials`, which authentication also
  uses, now allows `:` in passwords, as memcached's auth file splits each
  line at its first `:`, and rejects NUL in usernames and passwords, so
  connecting with a NUL in either now throws
  `ClientError InvalidAuthentication` before anything is sent. An IPv6
  address in brackets, as in `memcached://[::1]:11211`, now gives the host
  `::1`, without the brackets, and the `%25` that introduces a zone ID is
  decoded, so `[fe80::1%25eth0]` gives `fe80::1%eth0`. A path after an empty
  host, other than slashes alone, as in `memcached:///run/memcached.sock`,
  is now a Unix socket (see New), where 0.4 ignored it and gave `127.0.0.1`;
  with a port, or with `%00` or a raw `@` in the path, such a URI now
  returns `Left`.
- `show` for an `Authentication`, and so for a `ServerSpec`, prints the
  password as `<redacted>`, and the error that `parseServerSpec` and
  `parseConfigurationEndpoint` give for an invalid URI no longer quotes it,
  so that passwords stay out of logs.
- `getManyRequest` in `Database.Memcache.Socket`, and the request builders
  that `keyedBatchOp` in `Database.Memcache.Cluster` takes, have a new `Int`
  argument: an opaque, which the request must send unchanged, in decimal, as
  its `O` flag, so that its reply can be matched to it (see Fixes).
  `getManyRequest` sends it in place of the `k` flag, so its replies no
  longer echo the key.
- `Database.Memcache.Client` exports more names (see New), such as `Key`,
  `Version`, `Server` and `defaultOptions`, and `Database.Memcache.Cluster`,
  `Database.Memcache.Server`, `Database.Memcache.Socket` and
  `Database.Memcache.Types` export new names too. A new name can clash with
  another in a module that imports one of these modules unqualified without
  listing the names it imports, even one that hides some with `hiding`.

## New

- Unix sockets, contributed by drlkf: a `ServerSpec` whose host begins with
  `/` connects to the Unix socket at that path, for a memcached on the same
  machine. `parseServerSpec` reads one from a URI with a path, other than
  slashes alone, after an empty host, such as
  `memcached:///run/memcached.sock`, or
  `memcached://user:secret@/run/memcached.sock` with credentials. The path
  is percent-decoded to bytes. A port or `%00` is an error, and so is an `@`
  not written `%40`, so that a URI with one slash too many, such as
  `memcached:///user:secret@cache.example`, isn't taken for a socket path
  holding the password. The client doesn't use a socket's port to connect,
  but the port still names the server when keys are routed: `"11211"`, which
  a URI gives, names the socket by its path alone, as libmemcached's
  `memcached_server_add` does, and `"0"` names it `path:0`, as
  libmemcached's `memcached_server_add_unix_socket` and pylibmc do.
  ElastiCache discovery fails, rather than connect to a socket on this
  machine, if the endpoint lists a node whose host is a path.
- Keys go to the same server as with other clients that use libmemcached's
  weighted ketama distribution, given the same host strings and ports in the
  same order and equal weights: libmemcached with
  `MEMCACHED_BEHAVIOR_KETAMA_WEIGHTED`, php-memcached with
  `OPT_LIBKETAMA_COMPATIBLE`, pylibmc with the `ketama_weighted` behavior,
  and twemproxy with `hash: md5` and `distribution: ketama` and its servers
  listed without node names. In the rare case that two servers have a point
  with the same value, libmemcached before 1.1, which php-memcached and
  pylibmc may be built with, and twemproxy may choose differently for the
  keys just below it. The new module `Database.Memcache.Ketama` (`Ring`,
  `ring`, `serverName`, `keyHash`, `owner`, `owners` and `ringPoints`) works
  out which server a key belongs to, and builds rings with weights, which
  clusters don't use. `Database.Memcache.Cluster` exports the default router
  as `ketamaServerForKey`, and `isAlive` for a custom router that skips
  servers marked failed, and `Database.Memcache.Server` exports `serverHost`
  and `serverPort`.
- `withClient` creates a client, runs an action with it, and closes the
  client's connections with `quit` when the action returns or throws.
- `setMany` and `deleteMany` store and delete many keys, pipelining the
  requests to each server as `getMany` does. `setMany` returns the new
  version of each key, grouped by server; `deleteMany` returns once each key
  has been deleted or found missing, without saying which. Their request
  encoders, `setManyRequest` and `deleteManyRequest`, are in
  `Database.Memcache.Socket`.
- `defaultOptions`, `defaultServerOptions` and `defaultServerSpec` name the
  values that `def` gives. They are exported from
  `Database.Memcache.Cluster`, `Database.Memcache.Server` and
  `Database.Memcache.Types` respectively, and all three from
  `Database.Memcache.Client`.
- `Database.Memcache.Client` also exports `parseServerSpec`, `Retries`,
  `ServerOptions(..)`, the abstract `Server` type, and `Key`, `Value`,
  `Initial`, `Delta`, `Expiration`, `Flags` and `Version`, the types its own
  functions use.
- For lower-level use: `closeAll` in `Database.Memcache.Server` closes every
  connection to a server, as `quit` does for each server of a client;
  `keyedOpExpecting`, `anyOpExpecting` and `allOpExpecting` in
  `Database.Memcache.Cluster` reject a reply whose code is none of those
  given, closing its connection, and `keyedOpExpectingOnce` also doesn't
  send its request again once it may have reached the server; and
  `checkIdle` in `Database.Memcache.Socket` checks, without waiting, that
  nothing is left to read on a connection and that the server hasn't closed
  it (on Windows, it only checks what has already been read).

## Fixes

- Fix `getMany` dropping keys, or returning one key's value as another's,
  when it couldn't match the key that memcached echoed in a reply: a key
  containing byte 0xa0, such as UTF-8 `"voilà"`; a key with a space or
  control byte once an increment has created it or made its value longer, or
  an append or prepend has rebuilt it; and a plain key that another client
  stored with the `b` flag. When such a key held a line break, the echo also
  ended the reply line inside the key, so `getMany` could return part of the
  key as another key's value, fail with `BadLength` and mark a healthy
  server dead, or end the batch early and leave the rest of its replies on
  the pooled connection, to be read as the replies to later requests.
  `getMany`'s requests no longer ask memcached to echo the key, and batch
  replies are now matched to their requests by memcached's opaque (`O`) flag
  instead. A reply that matches no request in its batch, which used to be
  dropped, or that comes out of order, now throws `UnexpectedResponse` and
  closes the connection; like any server failure, the batch is then sent
  again on a new connection if retries are left, and the failure counts
  towards marking the server dead.
- Reply lines are split into tokens on spaces alone, as memcached writes
  them, so a token that `recvResponse` returns keeps any other whitespace
  byte, such as a tab or 0xa0, and so do `stats` values.
- In single-key operations, `version`, `flush` and authentication, a reply
  with an unknown code, or one the request can't get, as when an earlier
  request's reply is still on the connection, now closes the connection
  instead of returning it to the pool out of step, where later requests
  would read each other's replies. It throws `UnknownStatus` or
  `UnexpectedResponse`, where some such replies used to be taken as an
  answer: as success by a store, `increment`, `decrement`, `delete`, `flush`
  or authentication, and as a miss by `get`, `gat` and `touch` for an `NF`,
  and by `cas`, `replace`, `append`, `prepend`, `increment`, `decrement` and
  `delete` for an `EN`. `flush` now stops at the first server whose reply it
  rejects, leaving the servers after it unflushed, where it used to check
  such a reply only after sending `flush_all` to every server (see Breaking
  changes). In `getMany`, `setMany` and `deleteMany`, a reply with an
  unknown code throws `UnknownStatus` and closes the connection; the batch
  is then sent again on a new connection if retries are left, and the
  failure counts towards marking the server dead. `getMany` used to skip
  such a reply, or throw for it without sending the batch again or counting
  the failure.
- memcached's capitalised out-of-memory errors,
  `Out of memory allocating new item` (from `increment` and `decrement`) and
  `Out of memory during read`, are reported as `OpError ErrOutOfMemory`,
  like the lowercase ones (see Breaking changes). They were thrown as
  `ProtocolError ServerError`, so the request was sent again and a full but
  healthy server was marked dead.
- An attempt that runs out of `optsServerTimeout` while still waiting for a
  free pooled connection is no longer blamed on the server: it is retried as
  before, but if it was the last attempt, the operation throws
  `ClientError Timeout` without marking the server dead. Running out of time
  later, while connecting, authenticating or using the connection, still
  counts against the server. A connection is now opened, and authenticated,
  once its request has a place in the pool, so a failed connect no longer
  leaves requests queued for the pool blocked until their own timeout, as it
  did with resource-pool 0.3.0.0 to 0.5.0.0. An `allOp'` operation must make
  its requests through the `Server` it is given for its timeouts to count
  against the server.
- A failed attempt that counts against the server now closes the server's
  idle pooled connections before the operation is retried or the server is
  marked dead, as a timeout already did. So with `soNumResources` of 2 or
  more, a memcached restart or memcached's `idle_timeout` no longer sends
  each retry to another closed connection, failing the operation and marking
  a healthy server dead. With resource-pool 0.2, an asynchronous exception,
  such as a `cancel`, that arrived while those connections were being closed
  could be lost, so that the request carried on as if it hadn't arrived, or
  cut the closing short, leaving some of them open; with later versions, it
  could still cut the closing short between two of the pool's stripes.
  `close` in `Database.Memcache.Server`, which closes them, now holds such
  an exception back until every idle connection is closed, as `closeAll`
  does, so they are all closed first, and the exception then ends the
  request.
- Retries no longer run with asynchronous exceptions masked. Like the first
  attempt, each runs in the caller's masking state, so `optsServerTimeout`
  and the caller's own asynchronous exceptions, such as a `cancel`, can
  interrupt a retry that keeps working without blocking.
- When a server's host name resolves to several addresses, the client tries
  each in turn, within the attempt's `optsServerTimeout`, and keeps the
  first connection that succeeds until it is closed; it used to try only the
  first address. If every address fails, the error is still the first
  address's. An address that doesn't answer uses up the attempt's time, so
  when the first address refuses and a later one doesn't answer, an attempt
  now throws `ClientError Timeout` where it used to throw the first
  address's error at once (see Breaking changes). A failed authentication,
  or a server version the client refuses, is not retried against another
  address. If the addresses are separate memcached instances, then while the
  first refuses connections, new connections go to the next one that
  accepts, and so to another cache, and stay with it until they are closed;
  give each instance its own `ServerSpec`.
- `quit` no longer sends memcached's `quit` command, so it never connects to
  a server, and it never throws, even when every server is marked dead. It
  closes the client's connections to every server, including those marked
  dead: idle ones at once, and each one in use as soon as its request
  finishes. An asynchronous exception, such as a `cancel`, that arrives
  while it closes them is held back until every server's idle connections
  are closed. A request that starts later, or is still waiting for a
  connection, or opening one, when `quit` runs, opens a new one, which stays
  open as usual.
- `ElastiCacheClient.newClient` no longer stops refreshing the node list
  after the first refresh that fails. A refresh that throws, gets an error
  reply or runs out of time keeps the current servers, and discovery tries
  again at the next interval. Its error is dropped, where the first refresh
  that threw used to end discovery and print its exception on stderr. Each
  refresh has the first discovery's time limit, `optsServerTimeout`, and
  connects to the endpoint afresh, looking its name up again, as a
  connection kept from an earlier refresh could lead to a node that
  ElastiCache has since replaced. A refresh also keeps the `Server`, and so
  the pooled connections and failure state, of each node still listed, where
  it used to replace every server and leave the old connections open. It
  closes the connections of a node no longer listed, or listed with another
  IP address, as when ElastiCache replaces it: idle ones at once, and one in
  use as soon as its request finishes, so that requests connect again,
  looking the node's name up afresh.
- ElastiCache discovery checks the endpoint's replies more strictly. A
  configuration that lists more than 1,000 nodes, the same host and port
  twice, or a node that isn't a plain host and port now fails discovery, as
  a reply that can't be parsed does: `ElastiCacheClient.newClient` throws a
  `StringException`, and a refresh keeps the current servers. A node isn't a
  plain host and port if its host or port is empty, if its host is an IPv6
  address in brackets, or if its host or port has a character, such as `/`,
  `?` or `@`, that would change the host or port or add credentials, as
  `h/x` on port 22122 used to give `h` on port 11211; an empty host used to
  give `127.0.0.1`, and an empty port failed only at the first request. A
  version of more than 64 bytes in the endpoint's version reply fails
  discovery too, where parsing it took time that grew faster than the square
  of its length. Discovery's errors escape the text they quote from the
  endpoint and cut it to 200 characters, as `displayException` does for a
  `MemcacheError`. The refresh thread runs with asynchronous exceptions
  unmasked, even when `newClient` is called with them masked, as in the
  acquire action of a `bracket`, so that each refresh's time limit can
  interrupt it.
- In `getMany`, `setMany` and `deleteMany`, a bare `EN` (a miss reply
  without an opaque) now counts as the reply to the next request. One after
  the reply to the batch's last request throws `UnexpectedResponse`, and so
  does a reply to a request that a bare `EN` was counted for, or to an
  earlier one, so a server that sends bare `EN`s without end can no longer
  keep a batch reading.
- `stats` now closes each connection it reads a reply from, instead of
  returning it to the pool, so a later request to each server opens a new
  one in its place. A key holding a line break, which `stats cachedump` can
  list as it is stored, could end the reply early as `stats` read it, and
  the rest of the reply was then read as the reply to a later request on the
  same connection. The bytes after such a line break are still read as lines
  of their own, so they can add entries or end the reply early, but `stats`
  no longer sends its request again or marks the server dead when they make
  an empty line, or a `SERVER_ERROR` line naming no status: after a reply's
  first line, it throws the error of any empty or error line once every
  server has answered.
- Numbers in replies (value lengths, CAS versions, flags and counter values)
  are read only as plain decimal digits that fit their type, as memcached
  writes them (see Breaking changes), where they used to be read leniently,
  as with `(5)` or `0x10`, or wrapped around to fit. A value length in any
  other form, or a `VA` reply without one, throws `BadLength`; such a `VA`
  reply used to read as a hit with an empty value. A CAS version, flags or
  counter value in any other form reads as 0, as a missing one does, so such
  a CAS skips the check when passed back to an operation that takes a
  `Version`.

## Documentation and development

- The README and `Database.Memcache.Client` explain why to create one client
  and share it, and say more about timeouts, retries and failover, including
  which requests are never sent twice. `Database.Memcache.Cluster` documents
  routing, failover and upgrading from 0.4, which the README sums up, and
  the README has a section on Unix sockets.
- The README and the docs say which error replies count against a server: a
  `SERVER_ERROR` whose text names no status, such as memcached's proxy sends
  when a backend fails, counts against the server as a timeout does, while
  `ERROR`, `CLIENT_ERROR` and a `SERVER_ERROR` that names a status are
  thrown at once without counting against it. So behind memcached's proxy, a
  request for a key on a failed backend marks the whole proxy dead: at once
  if it mustn't be sent twice, and otherwise once its retries get the same
  reply. The docs of `Timeout` and `NoServersReady` now say when each is
  thrown.
- The docs say what each resource-pool version does with `soNumResources`
  and `soNumStripes`. With 0.3, which the bounds allow, a pool has a stripe
  for each capability, whatever `soNumStripes` is, so a pool with the
  default options holds one connection for each capability, and a thread
  takes connections only from its own capability's stripe. With 0.4 or
  later, `soNumResources` is split between the stripes, and creating a
  client throws an `ErrorCall` if it is smaller than `soNumStripes`.
- New dependency: `cryptohash-md5`, for the MD5 hashes of the ketama ring.
  The `spec` test suite now also depends on `directory`, `hashable`, `time`
  and `unix` (except on Windows), and `integration` on `unliftio`.
- Expand the test suite. Routing is checked against the servers that
  libmemcached-awesome 1.1.4 chooses for the same keys, including its own
  test fixture, and `legacyServerForKey` against a copy of 0.4's router.
  Mock tests cover the new operations, replies out of step, retries,
  failover and pool timeouts, closing connections, connecting to each
  resolved address, Unix sockets, and ElastiCache discovery and refreshes.
  The integration tests also run against memcached restarted, paused with
  `SIGSTOP`, listening on a Unix socket and out of memory, and test
  ElastiCache discovery against plain memcached.
- The `integration` test suite needs memcached 1.6.13 or later. With 1.6.10
  to 1.6.12, which the library accepts, two of its tests fail, as those
  servers answer an `append` or `prepend` with the version the request
  carried, or 0, rather than the new one. The documentation of `append` and
  `prepend` says so, as passing that 0 back as a version skips the check.
- The `parser` benchmark also times hashing a key, building a ring, finding
  a key's server on it and routing a key through a cluster.
- CI runs on the `ubuntu-26.04` runner image, named in the workflows instead
  of `ubuntu-latest`, installs the gold linker that older GHCs need there,
  keeps its caches per OS release, and uses `freckle/stack-action` 5.7.33.

# 0.4.2.1 - November 18th, 2026

No API changes from 0.4.2.0.

- `set`, `add`, `replace`, `cas`, `append` and `prepend` reject a value
  larger than 1GiB, memcached's largest possible item size, with
  `OpError ErrValueTooLarge` before sending anything. A value of about 2GiB
  or more (longer than 2147483645 bytes) previously made memcached run the
  value's bytes as commands, so storing untrusted data could run commands on
  the server, and a value over 1GiB could time out, be sent again and mark
  the server dead. Such a value no longer reaches the server, so it no
  longer makes memcached delete the key's existing value, as memcached does
  whenever it rejects a store as too large; a value within 1GiB but over the
  server's own limit (`-I`, 1MiB by default) still does. Every operation
  that sends a value now evaluates it first, before its timeout starts: an
  exception it throws is no longer retried and no longer marks the server
  dead, and time spent computing it no longer counts towards
  `optsServerTimeout`. The request encoders in `Database.Memcache.Socket`
  still encode a value of any length; don't send one longer than 1GiB.
- memcached 1.6.10 or later is now required (previously documented as
  1.6.0). memcached before 1.6.10 rejects the meta set command this library
  stores values with, without reading the value that follows it, and then
  runs the value's bytes as commands, so storing untrusted data could run
  commands on the server. Each new connection now asks the server for its
  version before it is used: until that check passes, the only requests sent
  are the authentication exchange, when credentials are set, and `version`.
  A server older than 1.6.10, or any other reply that is neither an error
  nor a readable version, is refused with `ProtocolError (UnknownOp ...)`,
  whose message quotes the reply; the connection is closed, and the refusal
  is not retried and doesn't mark the server dead. An error reply, such as
  `CLIENT_ERROR unauthenticated`, throws what it throws for any request, and
  so does a reply that can't be parsed, such as an empty line or an overlong
  one, which counts against the server. This covers ElastiCache discovery,
  which now fails for a cluster running a memcached older than 1.6.10.
  Through a proxy, the check sees only the proxy's version, so every
  memcached behind it must be 1.6.10 or later too. Each new connection that
  an operation opens takes one more round trip, which counts towards
  `optsServerTimeout`. Connections made directly with
  `Database.Memcache.Socket.newConnection` are not checked; send
  `storeRequest` and `modifyRequest` only to 1.6.10 or later.

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
