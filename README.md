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
chunks, each of which is identified by a unique, non-zero identifier
up to 256 bits in length, and each of which may be declared to depend
on zero or more other chunks.  The content of each chunk and its
dependency relationships are immutable.  Two chunks may have identical
content and dependencies but different identifiers, but no two chunks
with different content or dependencies may share an identifier.

(todo: Is the immutability of chunks and dependencies going to be a
problem for some applications, e.g. ones that want to rearrange
dependency relationships without resending the content?  If so, we
could make the chunk content and graph structure independent.)

In order to share content with subscribers, a publisher may define one
or more channels, each of which has its own unique identifier and
which may be declared to point to a chunk via its identifier.  The
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
UDP header sizes, etc.  Another advantage of this relatively small
size is that it gives the application maximum flexibility for
prioritizing and multiplexing data for multiple channels over a single
connection.

Content exceeding the size limit must be fragmented into several
chunks bound by a chain of dependencies.  Additionally, if an
application needs to send a chunk which depends on more chunks than
can be declared in a single datagram, it may fragment the dependency
list into a set of empty chunks which only depend on other chunks and
declare the original chunk to depend on that set, repeating as
necessary.

A channel update consists of a channel identifier and the identifier
of the chunk to which it should point, or zero if the channel should
be deleted.  It also includes a sequence number, ensuring that
subscribers are not confused by out-of-order packet delivery, which
might otherwise cause new content to be overwritten by old content.

A publisher may either publish a new channel or update an existing one
by first sending any new chunks not already known by the receiver and
then sending a channel update to point it to the chunk representing
the "root" of the content to be published.  The chunks should be sent
in reverse order of dependency, i.e. no chunk should be sent before
all its dependencies have been sent.

(todo: are circular dependencies useful?  Do we need to explicitly
allow or disallow them?)

Identifiers are encoded using the same base 128 "varint" encoding used
in Protocol Buffers:
https://developers.google.com/protocol-buffers/docs/encoding#varints.
The publisher may generate successive identifiers in whatever way it
choses (e.g. incrementing, random, or checksum), provided no distinct
chunks are assigned the same identifier.  Subscribers should not rely
on the publisher using any specific strategy.

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

##### Relay Node Access Control

Although the state encryption/decryption layer ensures that the
content published to a channel cannot be read or modified by
unauthorized clients, the synchronization layer cannot by itself
distinguish between legitimate and illegitimate packets.  In
particular, while authorized subscribers can easily detect and discard
illegitimate packets at the decryption layer, relay nodes will not
have the keys necessary to do the same.  Also, the operators of a
given relay node may wish to impose storage and bandwidth quotas on a
per-user and/or per-channel basis, making some form of authentication
essential.

Fortunately, the Transport Layer Security (TLS) protocol provides
authentication and message integrity, solving these issues neatly.
However, because client authentication is not widely used e.g. by Web
browsers, a relay node should not rely on it.  Instead, the client
must present an authorization message to the relay node prior to
sending any other packets.  This message must be signed using a
private key corresponding to a public key trusted by the node for this
purpose and may contain a list of permissions granted to and quotas
imposed on the presenting client.  Thus, the node need not maintain a
list of per-user permissions and quotas locally; it need only verify
that a trusted authentication service has authorized the client as
specified in the message.

(todo: specify permission and quota format, including interval of
validity, etc.)

#### State Encryption/Decryption Layer

Above the state synchronization layer, applications are free to use
any encryption strategy they chose, including no encryption at all if
privacy is not desired.  For those which do require privacy, the
following describes a recommended design for the encryption layer,
ideally packaged as a reusable, auditable library.

The design centers on a block cipher, such as AES, combined with a
mode of operation, such as CBC or XTS, which takes a unique
initialization vector as input for each chunk of date encrypted.
Alternatively, a tweakable block cipher, such as Threefish, may be
used, in which case the initialization vector is replaced with a
tweak, and no additional mode of operation is needed.

For maximum security, it may be desireable to encrypt the content
twice with two separate ciphers and/or operation modes, which ensures
that it will remain private if one of the algorithms is cracked, at
which point the content can be re-encrypted with a new pair of
algorithms.  This won't help if both algorithms are found to be
vulnerable in a short amount of time, nor will it guarantee privacy if
the second algorithm is broken after the content is re-encrypted,
since an adversary might have saved a private copy of the content in
its originally-encrypted form.  However, it does make attacks
logistically more difficult.

Each chunk of content may be encrypted independently of the others,
using a channel-specific key, plus the chunk identifier as the
initialization vector or tweak.

##### Key Regression

(todo)

##### Key Storage and Distribution

(todo)

#### Application Layer

(todo: examples of apps and libraries, e.g. versioned, collaborative
document editing, live video broadcasting, Firebase-style application
state synchronization, etc.)
