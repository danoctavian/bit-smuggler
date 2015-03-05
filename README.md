bit-smuggler
============
unseen, unheard


## Description

### Intro
bit-smuggler is a tool designed to allow you to defeat internet censorship by
tunneling your network traffic through what appears to be a genuine bittorrent peer 
connection, fooling censorship firewalls into thinking it's harmless.

### Use case 

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
    - eg. GHCQ's TEMPORA surveillance system throws away all P2P according to these [snowden docs](http://cryptome.org/2014/06/nsa-spiegel-snowden-14-0618.pdf)

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

#### Details 

More details on the design of bit-smuggler [here](https://github.com/danoctavian/bit-smuggler/blob/master/DESIGN.md)

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

