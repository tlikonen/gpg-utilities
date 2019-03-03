GPG Graph
=========

**Draw web of trust graphs of a GnuPG keyring**


Introduction
------------

`gpg-graph` is a command-line tool which inputs data from `gpg` ([GnuPG][])
security tool and outputs graph data in [Graphviz][] format. Graphviz
can then be used to draw web of trust graphs from GnuPG public keys.

`gpg-cert-path` is a tool for finding the shortest certificate path(s)
between two keys. The output is data for [Graphviz][]. The maximum steps
between keys is limited to 5 which is [GnuPG][]'s default
`--max-cert-depth` option. Long distances between keys would take very
long time to find in large keyrings.

The programs are written in the Common Lisp language and require [Steel
Bank Common Lisp][SBCL] implementation. For drawing graphs the Graphviz
tool is required. Both should be available in common GNU/Linux
distributions (Debian: `apt install sbcl graphviz`).

[GnuPG]:    https://gnupg.org/
[Graphviz]: https://graphviz.org/
[SBCL]:     http://sbcl.org/


Examples
--------

Usage: `gpg-graph [key1 ...]`

    gpg-graph kernel.org | dot -Tpng >wot-dot.png
    gpg-graph kernel.org | neato -Tpng >wot-neato.png
    gpg-graph kernel.org | sfdp -Tpng >wot-sfdp.png

Usage: `gpg-cert-path <from-key> <to-key>`

    gpg-cert-path 80615870F5BAD690333686D0F2AD85AC1E42B367 \
        ABAF11C65A2970B130ABE3C479BE3E4300411886 | dot -Tpng path.png


Author and License
------------------

Author: Teemu Likonen <<tlikonen@iki.fi>>

PGP: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

No restrictions for use: this program is placed in the public domain.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
