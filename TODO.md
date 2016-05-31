## ToDo

Optional:
* Typeclass abstracting key and values
* MonadIO?
* Safe Haskell
* Differentiate between two expiration modes?
* Simpler interface?
* Configurable fail-over mode

Protocol:
* Multi-get
* Generic multi operation support

Performance:
* Is System.Timeout still slow? Switch to Warp/Snap timeout handler?
* Is NominalDiffTime slow? Switch to unix epoch time?

Nice-to-have:
* Smarter connection handling to minimize system calls (buffered)
* Customizable server sharding -- mod & virtual servers
* Server weights
* Asynchronous support?
* Customizable -- timeout, max connection retries, hash algorithm
* Max value validation
* Optimizations --  http://code.google.com/p/spymemcached/wiki/Optimizations
* UDP
* ASCII
* Server error handling mode where we return misses and ignore sets

Maybe:
* Automatic encoding and decoding of Haskell data types (using flags?)

