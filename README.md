bit-smuggler
============
unseen, unheard


## Description

### Intro

China started [killing VPN connections](http://www.bbc.com/news/technology-30982198).

watcha gonna do.

bit-smuggler might be the tool for you. keep those pesky internet censors off your back, get your tweets through and read your wikipedia in peace.

bit-smuggler is a tool designed to allow you to defeat internet censorship by
tunneling your network traffic through what appears to be a genuine bittorrent peer 
connection, fooling censorship firewalls into thinking it's harmless.

### Use case 

 ![alt text](https://github.com/danoctavian/bit-smuggler/blob/master/docs/SmuggleTraffic.png "smuggler")


The projects was designed to be used as a [Tor pluggable transport](https://www.torproject.org/docs/pluggable-transports.html.en).

More generally, it's a tunnel between a proxy server and a client connecting to it that is
undetectable and unblockable in real-time by censorship and provides data confidentiality.

Currently it is not integrated with Tor (coming soon)
but it can be used to run a server acting as a
1-hop proxy and clients that provide a socks4 interface for apps to tunnel their
network traffic through the proxy.

### Design

### In a nutshell

bit-smuggler uses real uTorrent clients to establish a genuine-looking bittorrent connection between the
client and proxy and then tampers with the data exchanged by the 2 bittorrent clients
to inject and read its own payload into/from the bittorrent piece messages
(bittorrent's messages that contain the actual data of the file being transferred).

Currently, bit-smuggler uses a weak form of steganography by
simply substituting the file data with
cipher-text and relying on the fact video codecs (the most popular data type on 
bittorrent) look almost like random strings, just like ciphertext therefore it's hard
to distinguish between the 2.

#### Why bittorrent as cover
Reasons:

  * lots of it - high volume, many users -> hard, expensive to do surveillance on it
    - eg. GCHQ's TEMPORA surveillance system throws away all P2P according to these [snowden docs](http://cryptome.org/2014/06/nsa-spiegel-snowden-14-0618.pdf) (see page 183)

  * normally not banned in censoring countries (because fuck copyright)

  * naturally high throughput upstream/downstream

  * steganography - most data exchanged through bittorrent is video files (which is compressed -> high entropy -> stick your cipher text in it)

  * looks harmless
    - bittorrent peers on the internet connect to each other exchanging high volumes of data, what's the problem?!
    - a bit-smuggler proxy taking reasonably many connections doesn't the blow the cover either (your bittorrent client works on many files concurrently)

#### Why use real bittorrent clients

Why bother using real bittorrent clients and not just immitate the traffic or replay
modified bittorent traffic? This decision made bit-smuggler signficantly more complex so
here are some reasons:

A common attack agains Tor bridges used to unmask their dientity is to **active probe**
them see bottom of page
[here](https://trac.torproject.org/projects/tor/wiki/doc/PluggableTransports).
Using real bittorrent clients means that probing attacks are hard to get right since
the proxy (Tor bridge or just a proxy) responds like a normal bittorrent client since
it actually runs one.

Also, simply "parroting" the cover traffic has been discussed and shown to be very tricky
to get right as shown [here](http://www.cs.utexas.edu/~shmat/shmat_oak13parrot.pdf)

#### Problems

#### *Security*

Slow path analysis will break the undetectability of bit-smuggler.

**Bittorrent file integrity is broken**. Since we are tunnelling data through a bittorrent
file exchange in real-time we are altering file pieces and breaking the checksums 
of the file pieces.

To actually detect this in real time you need to do TCP packet reconstruction, fetch
the original torrent file, compute hashes and detect a high occurence of hash fails.
Sounds like a lot of expensive work. 

Sounds totally doable on a slow-path where you've recorded all the packets of a bittorrent
connection previously. 

#### *Performance*
Has poor latency since the stream of data is broken down to fit in bittorrent piece messages. (high throughput on the upside though)

## Install instructions

Instructions are Ubuntu specific and this has been verified to work on
Ubuntu 12.04 correctly.

The code of the project is in the BitSmuggler dir. Please ignore ./scripts

Haskell dependencies:

* ghc  7.8.2 -> the compiler

* cabal 1.20.0 -> the package system; greater version is also good. i know empirically this 1 works well

Your os distro might have them in its repositories or it might not. 

For **GHC**, go here https://www.haskell.org/ghc/download_ghc_7_8_2 and get the binary for your distro.

For **Cabal** do the following:

```bash
sudo apt-get install cabal # or equivalent for your distro
cabal update
cabal install cabal-install 
# this last command should have installed the latest version of cabal in ~/.cabal/bin
# give priority to that executable; you can add the following to your .bashrc or whatever
export PATH=$HOME/.cabal/bin:$PATH 
```

I highly recommend using sandboxes for haskell code. They are like virtualenvs for python. 
The following instructions and the setup.js script assume you will use sandboxes. sorry abouthat.

Script dependencies:

```bash
sudo apt-get install nodejs npm
sudo npm install shelljs # js script dependencies
```

To setup the project do the following (starting from root of the repo)

```bash
# cd in the root dir of the repo
# assumes you will use cabal sandboxes 
nodejs setup.js 
cd BitSmuggler

# this install haskell dependencies in a haskell sandbox that setup.js created
# yeah, i'm forcing you to use sandboxes. don't question this ok?
cabal install --dependencies-only --enable-tests

# to check that everything is good, just build.
cabal build
```


Testing notes 
-------------

There are 2 test suites:

  * **unit** - (run fast)

  * **integration** - showcases full bitsmuggler functioning
                  runs really slooooow the first time (generates randomized files to 
                  torrent later)
                  runs faster the second time around (they are cached) but still pretty slow
                  to start up because the bittorrent clients need to find each other
                  (even though they both run on localhost)

To run each test suite, run:

```bash
cabal test unit
# OR
cabal test integration 
# for both simply run:
cabal test
```

There's currently an issue with the integration test which just hangs once its done. so you need to ctrl-c out of it.

Deployable proxy app 
--------------------

Coming soon. An easy to setup server and client that work together.


## DESIGN (expanded)

## BitTorrent cover

The design of how to hide behind bittorrent was created with the following goal in mind:
make it look as genuine as possible in order to blend in with all other bittorrent
connections.

Bit-smuggler never makes any network connections by itself. All connections
between a bit-smuggler client and server are tunneled through connections
created by the bittorrent clients living on both sides.

Both the client and server each run exactly one instance of a bittorrent client.

The server runs a bittorrent client that presents itself as a peer holding a 
set of of incomplete files (contact-files). 

The client knows the server descriptor:

```haskell
data ServerDescriptor = ServerDescriptor {
    serverAddr :: IP 
  , contactFiles :: [ContactFile]
  , serverPubKey :: Key
}
```

The  bit-smuggler's client bittorrent client holds incomplete copies of the contact files as well.
The incomplete copies of a contact file F owned by both the client and the server
are not the same so that it allows the 2 peers to exchange pieces back-and-forth.
(each peer needs some pieces that the other has and has some pieces that the other doesn't)

The bit-smuggler client picks at random a contact file to download and instructs its
bittorrent client to go for it. the bittorrent client will eventually connect to its
peer (the bitsmuggler server) and a bitsmuggler connection can be established using that
data exchange as a tunnel. 

When the file being exchanged is exhausted (no more data flows back and forth
- all data that could be exchanged was exchanged),
the client requests to download another contact file at random.

The server constantly replenishes its exhausted contact files. it polls its bittorrent
client to see what files are exhausted. If it finds an exhaust it file, it deletes it
and readds it later, partially completed. so it can be reused for another
bit-smuggler session.

### Contact files

Contact files are currently random data files of various lengths generated with
a pseudorandom-generator from a single integer seed. 
both client and server generate these files ahead of time knowing the integer seed and the length, before establishing a connection.


They can be real files but that involes pulling the data of the respective file
(eg. Pirates of the Carribean) ahead of time.

This is considered good enough since, as discussed in the README.md, it doesn't make it
easier to detect in real-time and bit-smuggler will be detected anyway on a slow path.

File names are manually created but can be random generated perhaps using 
markov chains (further work).

Partial copies are generated by randomly selecting pieceCount / 2 pieces and zeroing them
out (removing the data). This means that each peer will get from the other roughly
1/4 of a file. So to exhaust a 4GB file, roughly 1GB will flow in both directions.

The server descriptor's metadata for each file consists of its torrent file and its integer seed. Currently the torrent file is distributed in its entirety but if you strip out the
piece hash data, a file's metadata will be down to 100 bytes (order of magnitude). For a server that holds 100 contact files is 10Kb order of size for a server descriptor. The question is open if this is acceptable or not.

### bit-smuggler session and disconnects

Since we want a long lived connection whose duration is independent of the duration of
downloading the files, we have the concept of a bit-smuggler session that can span over
multiple bittorrent file downloads.

Upon the first accept of a connection by the client, the server presents it with 
a *session token* (a cookie of sorts) that the client needs to use when 
disconnects happen to recover it's old session.

Suppose a bittorrent file is exhausted and the clients disconnect. The bit-smuggler
client commands its bittorrent client to pull another file from its peer (the server)
and when this reconnection happens it presents the session token in the handshake.
Based on it, the server pulls out the old session and starts streaming data normally.

The server keeps a session active around for a certain amount of time.

## Crypto

We are using symmetric key cryptography.

We are *assuming* the client **knows** the  public key of the server and sends 
its own  public key in plain but transformed so it looks like a random string
with djb's [elligator](http://cr.yp.to/elligator/elligator-20130527.pdf)

then they both derive a shared secret and use that to encrypt the rest of the 
communication. (see the crypto code and a diagram [here](https://github.com/danoctavian/bit-smuggler/blob/master/BitSmuggler/src/Network/BitSmuggler/Crypto.hs) )


 ![alt text](https://github.com/danoctavian/bit-smuggler/blob/master/docs/crypto.png "Crypto")

## System architecture

BitSmuggler consists of a client and a server. Both the client and the server run a bittorrent instance and capture its traffic using socks (for outgoing connections) and a 
reverse proxy (for incoming connections). 

The Bitsmuggler process orchestrates the whole thing sending commands to the bittorrent clients and having hooks to read/write the tcp streams passing through the sockets.

See a diagram for system components below.

 ![alt text](https://github.com/danoctavian/bit-smuggler/blob/master/docs/system-components.png "System architecture")

See [diagram](https://github.com/danoctavian/bit-smuggler/blob/master/docs/system-components.jpg) for a visual representation of the components of the system.
The reverse proxy works by setting IPTABLES rules to redirect all traffic aimed at the
bittorrent port to the port of the proxy, which will then redirect itself to the bittorrent
client.

```bash
# this sends all traffic aimedat 6881 to the proxy listening locally on 4001
sudo iptables -t nat -A PREROUTING -p tcp --dport 6881 -j DNAT --to 127.0.0.1:4001
```


## Protocols

The client and server talk by storing data in the data section of [BitTorrent piece messages](https://wiki.theory.org/BitTorrentSpecification#piece:_.3Clen.3D0009.2BX.3E.3Cid.3D7.3E.3Cindex.3E.3Cbegin.3E.3Cblock.3E)

Therefore, the channel for the bit-smuggler protocols is packet-based, with the 
reliability properties of TCP.

The reliability properties can be broken by an adversary that can tamper with the 
application level protocol.

*Example attack*: he can flip a bit in a bit-smuggler piece message,
which may contain a bit-smuggler message - thus breaking the cipher-text which
cannot be decrypted once it reaches the other side and the packet is lost.

The bit-smuggler protocol stack starting from the bottom (now we're here):

* **TCP**

* **BitTorrent**

* **encryption** = not a protocol but the encryption operation is applied over the whole data block stored in a bittorrent file piece

* **bit-smuggler ARQ** = a low overhead Automatic repeat request protocol that operates on packets embedded in piece messages ensuring reliable delivery. The reasons to include this in the stack are debatable:

    1. It protects against an adversary performing tampering attacks as described above. This is questionable since if the adversary can tamper at application level he may be able to do an analysis that detects bit-smuggler in real-time
    2. the implementation easily deals with disconnect events implicitly (switching from 1 file to another means 1 packet may be lost in flight). this can be achieved with a less complicated logic
    3. it's low overhead (small headers, rarely resends, sticks ack numbers on top of normal data packets to minimize the number of piece packets used)

* **bit-smuggler Wire** - length prefixed messages

* **bit-smuggler data-control** - 2 types of messages - data and control. control takes
precendence in the send queue over data. control are handshakes accepts and any other
potential instructions added later on (eg. server tell client to switch swarm)

* **application** - the user is provided with same connection interface given by TCP
 (a stream of data) and can build any application-level protocol on top of that.

## Threat model

The adversary (censor) has the following attributes:

* is a state-level authority

* has an interest in connecting to the Internet and allowing the individuals sub-
jected to censorship to connect to parts of the Internet for the socio-economical
advantages it provides. It is thus interested in avoiding over-blocking
• the state-level adversary has control over a section of the global network which we
will call the national network delimited by its national borders. We shall call this
delimiting perimiter its network perimeter.

* has full active and passive control over the national network. The censor can
passively monitor all traffic entering and leaving its networks. It is possible for the
censor to actively tamper with traffic by injecting, modifying or dropping it or just
hjacking TCP sessions

* performs Internet traffic filtering at the network perimeter based on: address filters,
content pattern filters, statistical filters; uses Deep-packet-inspection (DPI) to look
at the contents of the packets

* can be suspicious of encrypted network connections (ssh, vpn protocol, https, Bit-
Torrent encryption, tls) and may choose to block them without a strong justifica-
tion

The adversary’s objectives are:

* block access to certain Internet resources

* monitor all Internet usage
  - know which parties are communicating

  - know the content/type of their communication

The adversary’s *assumed* limitations are:

* DPI real-time analysis has strong time limitations for the computations that can be
performed while on-the-side anlysis forces the censor to select a very small portion
of the traffic for more complex analysis

* does not have access to the censored users’ machines. Alhough the censor may issue
mandates to inspect user devices or infect user devices with surveillance software,
BitSmuggler cannot service the user reliably if his/her machine is compromised

* has very limited abilities outside its network. It does not control any external
network infrastructure or any popular external websites.

* is subjected to economical constraints - improving its surveillance infrastructure
to counter new circumvention methods takes time and money

* has strong socio-economic reasons not to block BitTorrent traffic

* should Tor be the proxy, it does not own enough malicious Tor relays to allow it to land attacks which compromise Tor’s unlinkability

## ODDITIES - how bit-smuggler uses bittorrent in unusual ways

This is a list of ways in which bit-smuggler uses bittorrent in strange ways that **may allow an adversary to fingeprint it**. It's up for debate whether these are acceptable trade-offs or not.

1. **Breaks .torrent file checksums** The data being transmitted in a bittorrent file exchange
which is tampered with by bit-smuggler does not match checksums specified in the .torrent file. This is because bit-smuggler inserts encrypted payloads in real-time, and it cannot find a hash collision to match the checksum or pre- prepare the file so that checksums match. As stated this allows an adversary to detect a bit-smuggler connection if he manages to reconstruct the TCP stream, compute the hashes and check against the pieces reassembled from the stream.

2. **File content**.  files are just random data, generated with a pseudo-random generator using an integer seed. So an entropy analysis of the the file may be a give-away (the fact that it doesn't look like anything really) and also the fact that right now the percentage of the file that is available is fixed (1/2). 

a solution here is to use real existing files, but this involves pre downloading them (fetch pirates of the Caribbean 3 first and then torrent it again with your bit-smuggler server).  how much of the file is available can be randomized.


3. **Contact files**. Another aspect is that the server now works by advertising a set of so called contact files. those contact files are bittorrent files that a client needs to start downloading to tunnel a bit-smuggler connection through them. They are partially completed files (1/2 of the pieces are there). Once they are depleted (all downloadable pieces are downloaded) the file is removed and a new partial copy is placed in, to allow for new peer connections on that contact file to have plenty of data flowing back and forth. 

This aspect that the server keeps refreshing its files is odd.

These files are part of the server descriptor.

Another way of doing it could be deciding the contact files dynamically. You could maybe have a small exchange at the very beginning between the server and the client through some other channel and steg some data in there.

Possible ways:
* the client can make a DHT request for the server, and the server would reply with a set of nodes, but the data in the reply contains data about what contact file the client should use, so not a correct DHT query response.
* ue the bifield message of bittorrent to do a request-response sequence between the bitsmuggler server and client about what contact file to use and then switch to it.

4. **Upload slots per torrent = 1** . the client and server instruct their bittorrent clients to upload to a single peer. basically i'm restricting swarms to a size of 2 to load balance things. if i disable this it would just mean the file gets depleated faster. 

So actually, given this setting, an outsider joining a swarm where a bit-smuggler server and client live would not actually be able to download.





