GPG Utilities
=============

**Utilities for GnuPG security tool**


Programs
--------

### GPG Tofu

`gpg-tofu` is a tool which inputs public key data from `gpg` ([GnuPG][])
security tool and outputs "trust on first use" (TOFU) statistics about
the keys.

Usage: `gpg-tofu [key1 ...]`

    $ gpg-tofu tlikonen@iki.fi

    pub 4E1055DC84E9DFF613D78557719D69D324539450
    uid [ultimate] Teemu Likonen <tlikonen@iki.fi>
        TOFU validity: (4/4) a lot of history for trust, TOFU policy: good
        430 signatures in 1 year 254 days, first: 2017-06-09 11:28:16Z, last: 2019-02-18 14:18:37Z
        404 encryptions in 1 year 244 days, first: 2017-06-15 14:41:30Z, last: 2019-02-14 19:25:41Z
    [...]


### GPG Graph

`gpg-graph` is a command-line tool which inputs data from `gpg` ([GnuPG][])
security tool and outputs graph data in [Graphviz][] format. Graphviz
can then be used to draw web of trust graphs from GnuPG public keys.

Usage: `gpg-graph [key1 ...]`

    $ gpg-graph kernel.org | dot -Tpng >wot-dot.png
    $ gpg-graph kernel.org | neato -Tpng >wot-neato.png
    $ gpg-graph kernel.org | sfdp -Tpng >wot-sfdp.png


### GPG Cert Path

`gpg-cert-path` is a tool for finding the shortest certificate path(s)
between two keys. The output is data for [Graphviz][]. The maximum steps
between keys is limited to 5 which is [GnuPG][]'s default
`--max-cert-depth` option. Long distances between keys would take very
long time to find in large keyrings.

Usage: `gpg-cert-path <from-key> <to-key>`

    $ gpg-cert-path 80615870F5BAD690333686D0F2AD85AC1E42B367 \
        ABAF11C65A2970B130ABE3C479BE3E4300411886 | dot -Tpng >path.png

[GnuPG]:    https://gnupg.org/
[Graphviz]: https://graphviz.org/


How to Build and Install
------------------------

The programs are written in the Common Lisp language and require [Steel
Bank Common Lisp][SBCL] implementation for compiling. For drawing graphs
the [Graphviz][] tool is required. Both should be available in common
GNU/Linux distributions (Debian: `apt install sbcl graphviz`).

To build the necessary files run this command:

    $ make

To install the files in the default locations run this command:

    $ make install

`make` command will also save some configuration variables to
`config.mk` file. The default installation target is `~/bin` for
executable files (actually symbolic links) and `~/.local/lib` directory
for the common library file. You can configure different locations with
Makefile variables and save them to a configuration file for later runs.
For example:

    $ make clean-all
    $ make config bindir=/usr/local/bin libdir=/usr/local/lib
    $ make
    $ sudo make install

To uninstall all files run command `make uninstall` with the same
configuration as during installation. You can clean build files with
`make clean` or `make clean-all` (which deletes also the configuration).

[SBCL]:     http://sbcl.org/


Author and License
------------------

Author: Teemu Likonen <<tlikonen@iki.fi>>

PGP: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

No restrictions for use: this program is placed in the public domain.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
