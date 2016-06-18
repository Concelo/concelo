Concelo - Real-time synchronization of encrypted content
========================================================

[![Build Status](https://travis-ci.org/Concelo/concelo.svg?branch=master)](https://travis-ci.org/Concelo/concelo)

Concelo is a (currently hypothetical) system for synchronizing
encrypted application state among multiple peers.  Its goals include:

  * `Scalability`: networks of cloud-based relay nodes allow
    publishers to broadcast to large numbers of subscribers without
    maintaining per-subscriber state or connections

  * `Privacy`: relay nodes have no means to decrypt or modify the
    content they relay

  * `Reliability`: a graph-based state synchronization protocol
    ensures that subscribers will eventually converge on the published
    state regardless of intermittent packet or connection loss, IP
    address changes, etc.

  * `Low latency`: the synchronization protocol naturally allows for
    obsolete elements of state to be skipped as newer data becomes
    available

Please see the [wiki](https://github.com/Concelo/concelo/wiki) for details.


Build Notes
-----------

```

  # First, install ghc+cabal:
    # For linux:
    sudo apt-get install ghc-prof libghc-mtl-prof libghc-text-prof libghc-parsec3-prof libghc-network-prof

    # For mac:
    brew install cabal-install

  # Make a cabal sandbox so as to not pollute the global package store
  cabal sandbox init

  # Finally, compile & run the tests:
  cabal test --show-details=streaming
```
