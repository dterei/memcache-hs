# 0.2.0.0 - 

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

