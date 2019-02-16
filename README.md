GPG-graph
=========

**Draw web of trust graphs of a GnuPG keyring**


Introduction
------------

`gpg-graph` is a command-line tool which inputs data from `gpg` ([GnuPG][])
security tool and outputs graph data in [Graphviz][] format. Graphviz
can then be used to draw web of trust graphs from GnuPG public keys.

The program is written in the Common Lisp language and requires [Steel
Bank Common Lisp][SBCL] implementation. For drawing graphs the Graphviz
tool is required. Both should be available in common GNU/Linux
distributions (Debian: `apt install sbcl graphviz`).

[GnuPG]:    https://gnupg.org/
[Graphviz]: https://graphviz.org/
[SBCL]:     http://sbcl.org/


Examples
--------

Usage: `gpg-graph [key1 ...]`

Examples:

    gpg-graph kernel.org | dot -Tpng >wot-dot.png
    gpg-graph kernel.org | neato -Tpng >wot-neato.png
    gpg-graph kernel.org | sfdp -Tpng >wot-sfdp.png


Author and license
------------------

Author:  Teemu Likonen <<tlikonen@iki.fi>>

PGP: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

License: Public domain

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
