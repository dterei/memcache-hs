# Security Policy

## Supported Versions

Security fixes are made for the latest release series, currently 0.4.x, and
released as a new version of it on Hackage. Older series don't receive fixes,
so please upgrade to the latest 0.4 release.

| Version | Supported          |
| ------- | ------------------ |
| 0.4.x   | :white_check_mark: |
| < 0.4   | :x:                |

## Reporting a Vulnerability

Please report vulnerabilities privately, not in a public issue or pull request.

- Email David Terei at <code@davidterei.com>. This always works.
- If GitHub's private vulnerability reporting is enabled for this repository,
  you can use it instead: open the repository's **Security** tab and choose
  **Report a vulnerability**, or go to
  <https://github.com/dterei/memcache-hs/security/advisories/new>. If there is
  no such button or the link doesn't work, use email.

Please include the affected versions, a description of the problem and its
impact, and the steps or code needed to reproduce it.

## What to Expect

This library is maintained by a volunteer, so reports are handled on a best
effort basis, without guaranteed response times. You can expect:

- an acknowledgement once the report has been read, and updates as it is
  investigated;
- if the issue is confirmed, a fix prepared privately and released on Hackage,
  together with a GitHub security advisory crediting you, unless you'd rather
  not be named. Where it applies, the advisory is also submitted to the
  [Haskell security advisory database](https://github.com/haskell/security-advisories).

Please keep the details private until a fixed release is available, or until
we have agreed on a disclosure date.

## Scope

This policy covers the `memcache` library. Vulnerabilities in memcached itself
should be reported to the [memcached project](https://github.com/memcached/memcached).

This library connects to memcached without TLS, so credentials sent for `-Y`
authentication and all data can be read by anyone on the network path. Run
memcached on a trusted network. Missing TLS support is a known limitation of
this library, not a vulnerability.
