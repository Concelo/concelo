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


Problem Space
-------------

#### Privacy

Modern multiuser applications such as social networks, online games,
and collaboration tools must all solve the same fundamental problem:
shared state must be reliably and promptly synchronized among
participating peers.  If this process is not both reliable and prompt,
a subset of those peers may end up stuck with an outdated or
inconsistent view of the shared content, leading to confusion and
frustration.

One common solution to this problem is to store the content in a
server or cloud of servers, accessible via thin clients.  This model
gives the service provider maximum control and flexibility, allowing
it to scale resources in response to demand and protect client data
via redundancy.  It's also convenient for users: data in the cloud is
accessible from any device at any location, even if it was originally
created on a device which has become inaccessible.

Another advantage of the cloud model is that service providers can
analyze the data they collect and use it to provide enhancements like
predictive search and audio-visual mixing and transcoding.  The
provider itself may also benefit by mining the data for marketing or
other purposes.  This model can also provide a degree of "stickiness"
if the data cannot be easily exported or transferred to a different
provider.  Finally, data stored in the cloud is generally more
accessible to law enforcement organizations than data stored on
individual devices, especially as end-user storage encryption becomes
more common.

Unfortunately, many of the advantages of cloud storage can also be
considered disadvantages.  The ubiquity and persistence of this data
makes it an attractive target for rogue employees, hackers, oppressive
governments, ex-spouses, etc.  Even the service provider's own
management and investors must balance their customers' best interests
with those of the company, and may not always put the former ahead of
the latter.  Furthermore, although a company may be genuinely
committed to being a good steward if its customer's data, it may need
to store that data in third party datacenters and shared hosting
environments outside of its direct control.  And while the
"stickiness" of cloud-based storage makes it easier for a company to
retain customers, it's a liability for users who may wish to change
providers without losing access to their library of content.

#### Coherency

In addition to privacy concerns, service providers also face the
challenge of keeping data synchronized among multiple collaborating
users, each using client software which may have its own cached view
of the data.  These clients may have only intermittent and unreliable
connections to the cloud and must handle temporary disconnects as
gracefully as possible, ideally allowing a user to work offline if
applicable.  Finally, the data may need to be synchronized across
servers within the cloud for availability and redundancy.

These challenges are often addressed on a per-application basis using
custom logic tailored to the structure of the data in question.  The
application-specific nature of this logic can be a liability as it
must be updated and regression tested each time the data structure
changes, a task made especially daunting by the non-deterministic
nature of changing network conditions.

More general solutions to this problem exist, such as Firebase and
PubNub (todo: open source options?), but suffer from many of the
privacy issues described above due to lack of end-to-end encryption.

There are also a few end-to-end encrypted document storage solutions
available such as Spideroak and Wuala (todo: open source options?),
but they provide little or no support for real-time, collaborative
data synchronization.  Moreover, these services rely on opaque,
closed-source client applications and protocols which cannot be
independently audited for security, nor can they be reused in other
applications.


Design
------

Concelo's design consists of three layers, listed below from lowest to
highest:

  * `State synchronization`: a system and protocol for keeping
    subscribers in sync with published content without allowing
    intermediaries to access or modify unencrypted content

  * `State encryption/decryption`: a library and protocol for end-to-end
    encryption of content and application state

  * `Application`: logic and user interface for accessing and
    modifying content

#### State Synchronization Layer

This layer is responsible for synchronizing publishers and subscribers
in a scalable way without any knowledge of the structure or semantics
of the data it is synchronizing.

##### Data Structures

The fundamental unit at this layer is a chunk of opaque, encrypted
content emitted by publishers and consumed by consumers.  A given
application might break up its content into an arbitrary set of these
chunks, each of which is identified by a unique, non-zero, 256-bit
identifier, and each of which may be declared to depend on zero or
more other chunks.  The content of each chunk and its dependency
relationships are immutable.  Two chunks may have identical content
and dependencies but different identifiers, but no two chunks with
different content or dependencies may share an identifier.

In order to share content with subscribers, a publisher may define one
or more channels, each of which has its own unique 256-bit identifier
and which may be declared to point to a chunk via its identifier.  The
pointer to the chunk is mutable and may change rapidly as new data is
published.

##### Publisher Protocol

The publishing protocol consists primarily of two kinds of datagrams:
chunks and channel updates.

The serialized form of a chunk consists of a protocol header (todo:
define this), the identifier, a list of dependency identifiers, and
the opaque content.  In order to allow chunks to be sent reliably as
IP datagrams, we further require that the serialized form is less than
the 1500 byte Ethernet maximum transmission unit, or MTU, minus IP and
UDP header sizes, etc.  Content exceeding this limit must be
fragmented into several chunks bound by a chain of dependencies.
Additionally, if an application needs to send a chunk which depends on
more chunks than can be declared in a single datagram, it may fragment
the dependency list into a set of empty chunks which only depend on
other chunks and declare the original chunk to depend on that set,
repeating as necessary.

A channel update consists of a channel identifier and the identifier
of the chunk to which it should point, or zero if the channel should
be deleted.

A publisher may either publish a new channel or update an existing one
by first sending any new chunks not already known by the receiver and
then sending a channel update to point it to the chunk representing
the "root" of the content to be published.  The chunks should be sent
in reverse order of dependency, i.e. no chunk should be sent before
all its dependencies have been sent.  (todo: are circular dependencies
useful?  Do we need to explicitly allow or disallow them?)

##### Subscriber Protocol

A subscriber may subscribe to one or more channels by sending a series
of packets, each of which is of one of the following types:

  * `Channel subscribe`: indicates interest in one or more channels,
    specified by their identifiers

  * `Acknowledgement`: provides a list of chunk identifiers already
    known to the subscriber

  * `Negative acknowledgement`: provides a list of chunk identifiers
    referred to directly or indirectly by one or more channels of
    interest but not yet received

A subscriber should only acknowledge a chunk if it has received both
the chunk itself and all its dependencies, whereas a negative
acknowledgement refers only to the chunks themselves and does not
imply that their dependencies are also missing.  Thus, there is no
need to explicitly acknowledge both a chunk and its dependencies, as
the publisher will infer that the subscriber has all the direct and
indirect dependencies of acknowledged chunks.  This allows
reconnecting subscribers to sync very quickly in cases where the
published content has not diverged much from its last known value --
the publisher need only send chunks which have not been acknowledged
explicitly or implicitly.

##### Relay Nodes

In addition to exchanging packets directly, publishers and subscribers
may also communicate indirectly via one or more relay nodes.  These
nodes can provide several benefits:

  * `NAT and firewall traversal`: clients unable to communicate
    directly may be able to do so via a mutually-accessible relay

  * `Scalability`: a bandwidth- or resource-constrained publisher may
    publish to a relay which directly or indirectly feeds a very large
    number of subscribers

  * `Persistence`: a relay can persist the most recently-updated
    content of a channel indefinitely until and unless an authorized
    publisher chooses to update or delete it, making it available to
    subscribers even after the publisher has disconnected

A relay node acts as a subscriber while communicating with a publisher
and vice-versa.

#### State Encryption/Decryption Layer

(todo: using public key cryptography and key regression to control
access to published content end-to-end)

#### Application Layer

(todo: examples of apps and libraries, e.g. versioned, collaborative
document editing, live video broadcasting, Firebase-style application
state synchronization, etc.)
