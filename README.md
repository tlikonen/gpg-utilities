GPG Utilities
=============

**Utilities for [GnuPG][] security tool**


The Programs
------------

### GPG Tofu

`gpg-tofu` is a command-line tool which inputs public key data from
[GnuPG][] (`gpg`) security tool and outputs "trust on first use" (TOFU)
statistics about the keys. It is similar to `gpg --list-keys` command
but only with TOFU statistics.

Usage: `gpg-tofu [options] [--] [key1 ...]`

    $ gpg-tofu tlikonen@iki.fi

    pub 4E1055DC84E9DFF613D78557719D69D324539450 (expires: [...])
    uid [ultimate] Teemu Likonen <tlikonen@iki.fi>
        TOFU validity: (4/4) a lot of history for trust, TOFU policy: good
        523 signatures in 2 years 9 days (P0002-00-09T18:44:06)
          2017-06-09T14:28:16+03:00/2019-06-19T09:12:22+03:00
    [...]

Use `-h` option to print [help text](doc/gpg-tofu.txt).


### GPG Graph

`gpg-graph` finds how [GnuPG][] keys are connected by certificates (web
of trust) and outputs a graph data in [Graphviz][] format. Graphviz can
then be used to draw web of trust images.

Usage: `gpg-graph [options] [--] [key1 ...]`

    $ gpg-graph kernel.org | dot -Tpng >wot-dot.png
    $ gpg-graph kernel.org | neato -Tpng >wot-neato.png
    $ gpg-graph kernel.org | sfdp -Tpng >wot-sfdp.png

Use `-h` option to print [help text](doc/gpg-graph.txt).


### GPG Cert Path

`gpg-cert-path` is a tool for finding the shortest certificate path(s)
between two keys. The output is data for [Graphviz][].

Usage: `gpg-cert-path [options] [--] <from-key> <to-key>`

    $ gpg-cert-path 80615870F5BAD690333686D0F2AD85AC1E42B367 \
        ABAF11C65A2970B130ABE3C479BE3E4300411886 | dot -Tpng >path.png

Use `-h` option to print [help text](doc/gpg-cert-path.txt).


### GPG Count Steps

`gpg-count-steps` counts the steps of the shortest certificate path
between any two keys in the keyring. It can test all keys between each
other or just specified keys. If one key is given as argument it counts
certificate steps from that key to all other keys. If two keys are given
as arguments it counts steps just between those keys.

The output consists of lines with three fields:

 1. The fingerprint of the *from* key.
 2. The fingerprint of the *to* key.
 3. The number of steps between the keys (or "-" if connection wasn't
    found).

Usage: `gpg-count-steps [options] [--] [from-key [to-key]]`

Use `-h` option to print [help text](doc/gpg-count-steps.txt).


[GnuPG]:    https://gnupg.org/
[Graphviz]: https://graphviz.org/


How to Build and Install
------------------------

The programs are written in the Common Lisp language and require [Steel
Bank Common Lisp][SBCL] implementation for compiling and running. For
drawing graphs the [Graphviz][] tool is required. Both should be
available in common GNU/Linux distributions ([Debian][]: `apt install
sbcl graphviz`).

To build the necessary files run this command:

    $ make

To install the files in the default locations run this command:

    $ sudo make install

The default installation target is under `/usr/local` directory. You can
configure different directory hierarchy with makefile variable `prefix`
or separately for executable files with `bindir` and library files with
`libdir`. Variable `sbcl` defines the SBCL path and variable `gpg` the
GnuPG path. Variables are stored in `config.mk` file. Use the same
target location variables in compiling and installing.

    $ make distclean
    $ make sbcl=/usr/local/bin/sbcl bindir=~/bin libdir=~/.local/lib
    $ make install

To uninstall all files run command `make uninstall` with the same
configuration variables as during installation.

[SBCL]:   http://sbcl.org/
[Debian]: https://www.debian.org/


License and Source Code
-----------------------

Author: Teemu Likonen <<tlikonen@iki.fi>>

OpenPGP key: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

License: [Creative Commons CC0][CC0] (public domain dedication)

The source code repository:
<https://github.com/tlikonen/gpg-utilities>

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
[CC0]: https://creativecommons.org/publicdomain/zero/1.0/legalcode
