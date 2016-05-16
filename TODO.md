## ToDo

Required:
* Connection error handling - See Cluster.hs
  - Server abstraction should be memcache protocol agnostic, move all protocol
    to higher level and just have server as a connection management
  - Move to mutable type for `Cluster.servers`: MVector most likely but could
    consider:
    - TVar/IORef/MVar [Server]
    - TVar/IORef/MVar (Vector Server)
    - [TVar/IORef/MVar Server]
    - Vector (TVar/IORef/MVar Server)
  - Remove `FailSilent` error mode, handle at client layer
  - Pull `serverOp` into `keyedOp`
  - Add failed timestamp to `Server`
  - Implement `FailToError`
  - Implement `FailToBackup`
  - Move to passing in encoded bytes for transmision rather than a closure for
    `keyedOp`
* Timeouts

Optional:
* Multi-get
* Generic multi operation support
* Customizable server sharding -- mod & virtual servers

Nice-to-have:
* Asynchronous support
* Customizable -- timeout, max connection retries, hash algorithm
* Max value validation
* Optimizations --  http://code.google.com/p/spymemcached/wiki/Optimizations
* UDP
* ASCII
* Server error handling mode where we return misses and ignore sets

Maybe:
* Typeclass for serialization
* Monad / Typeclass for memcache

