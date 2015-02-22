bit-smuggler
============

WARNING:
not easy to run atm
working on a 1-command setup

pluggable transport for censorship circumvention

check out Chapter 1 intro in https://github.com/danoctavian/bit-smuggler/tree/master/docs/Report.pdf for an overview of the project.


Install instructions
--------------------

Instructions are Ubuntu specific and this has been verified to work on
Ubuntu 12.04 correctly.

```bash
sudo apt-get install ghc cabal nodejs npm # npm to run scripts
sudo npm install shelljs # js script dependencies
# cd in the root dir of the repo
# assumes you will use cabal sandboxes 
nodejs setup.js 
```

Testing notes 
-------------

There are 2 test suites:

  * **unit** - (run fast)

  * **integration** - showcases full bitsmuggler functioning
                  runs really slooooow the first time (generates randomized files)
                  runs faster the second time around but still pretty slow
                  to start up because the bittorrent clients need to find each other
                  (even though they both run on localhost)


