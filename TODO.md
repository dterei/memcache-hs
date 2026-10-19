## ToDo

Behavior and API fixes, including routing and virtual nodes, are tracked in
[#46](https://github.com/dterei/memcache-hs/issues/46).

Optional:

- Typeclass abstracting key and values
- MonadIO?
- Safe Haskell
- Differentiate between two expiration modes?
- Simpler interface?
- Configurable fail-over mode

Protocol:

- Review quiet multi-get behavior when a server closes before `mn`.

Performance:

- System.Timeout forks a thread per server request on the non-threaded RTS and
  on Windows; consider a shared timer.

Nice-to-have:

- Built-in modulo sharding (custom routing works through `optsGetServerForKey`)
- Server weights
- Asynchronous support?
- Max value validation before sending
- Server error handling mode where we return misses and ignore sets

Maybe:

- Automatic encoding and decoding of Haskell data types (using flags?)
