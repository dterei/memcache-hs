# 0.4.0.0 - March 20th, 2024

* Add `optsGetServerForKey` field to `Options`.
* Change how `sid` is calculated.
* Expose `getServers` function.

# 0.3.0.0 - January 17th, 2021

* Bump package dependencies for newer GHC/back/network.
* Update code to work with newer dependencies.

# 0.2.0.1 - November 2nd, 2016

* Fix compatability with latest `data-default-class`
* Add new ReqRaw type for external clients to implement custom requests. Quite
  a hack right now, so behind a WARNING pragma.

# 0.2.0.0 - May 27th, 2016

* Big design change to reduce code duplication (`Protocol` module gone).
* Remove `Options` type - just fixed configuration for now.
* Design change also allows proper retry handling on operation failure - we
  retry an operation against the same server, but after N consecutive failures,
  we mark the server as dead and don't try using it again until M seconds has
  passed.
* Simplify exception hierachy - just one type `MemcacheError` now for
* Remove `defaultOptions` and `defaultServerSpec`, will revist usefulness.
  exceptions.
* Remove many `Typeable` instances.
* Support better testing with a mock Memcached server.
* Fix bug in socket handling - detected EOF properly.
* Greatly improve documentation.
* Use `data-default-class` for defaults of servers and options.

# 0.1.0.1 - February 26th, 2016

* Consistent usage of 'memcached' instead of 'memcache'.
* Document `Database.Memcache.Client`.
* Add inline pragmas in appropriate places.
* Fix bug handling fragmented IP packets (Alfredo Di Napoli).

# 0.1.0.0 - May 18th, 2015

* First proper release (although still lots of rough edges!).
* Filled out `Data.Memcache.Client` to a complete API.
* Integrated cluster and authentication handling.
* Better error and exception handling.
* Fix compilation under GHC 7.10.

# 0.0.1 - May 5th, 2015

* Initial (incomplete) support for a cluster of memcached servers.
* Fixed compilation under GHC 7.4.

# 0.0.0 - August 23rd, 2013

* Initial release. Support for a single server.

