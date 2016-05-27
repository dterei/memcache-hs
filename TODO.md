## ToDo

Required:
* Fix < GHC710

Optional:
* Configurable fail-over mode
* Multi-get
* Generic multi operation support
* Customizable server sharding -- mod & virtual servers

Nice-to-have:
* Smarter connection handling to minimize system calls (buffered)
* Server weights
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
* Automatic serialization / deserialization

