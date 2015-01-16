Concelo - Real-time synchronization of encrypted content
========================================================

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

Please see the [wiki](https://github.com/dicej/concelo/wiki) for details.
