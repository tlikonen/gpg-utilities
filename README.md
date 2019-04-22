GPG Utilities
=============

**Utilities for [GnuPG][] security tool**


The Programs
------------

### GPG Tofu

`gpg-tofu` is a command-line tool which inputs public key data from
`gpg` ([GnuPG][]) security tool and outputs "trust on first use" (TOFU)
statistics about the keys. It is analogous to `gpg --list-keys` command
but only with TOFU statistics.

Usage: `gpg-tofu [options] [--] [key1 ...]`

    $ gpg-tofu tlikonen@iki.fi

    pub 4E1055DC84E9DFF613D78557719D69D324539450
    uid [ultimate] Teemu Likonen <tlikonen@iki.fi>
        TOFU validity: (4/4) a lot of history for trust, TOFU policy: good
        430 signatures in 1 year 254 days, first: 2017-06-09 11:28:16Z, last: 2019-02-18 14:18:37Z
        404 encryptions in 1 year 244 days, first: 2017-06-15 14:41:30Z, last: 2019-02-14 19:25:41Z
    [...]

Use `-h` or `--help` option to print help text.


### GPG Graph

`gpg-graph` finds how [GnuPG][] keys are connected by certificates (web
of trust) and outputs graph data in [Graphviz][] format. Graphviz can
then be used to draw web of trust images.

Usage: `gpg-graph [options] [--] [key1 ...]`

    $ gpg-graph kernel.org | dot -Tpng >wot-dot.png
    $ gpg-graph kernel.org | neato -Tpng >wot-neato.png
    $ gpg-graph kernel.org | sfdp -Tpng >wot-sfdp.png

Use `-h` or `--help` option to print help text.


### GPG Cert Path

`gpg-cert-path` is a tool for finding the shortest certificate path(s)
between two keys. The output is data for [Graphviz][].

Usage: `gpg-cert-path [options] [--] <from-key> <to-key>`

    $ gpg-cert-path 80615870F5BAD690333686D0F2AD85AC1E42B367 \
        ABAF11C65A2970B130ABE3C479BE3E4300411886 | dot -Tpng >path.png

Use `-h` or `--help` option to print help text.


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

Use `-h` or `--help` option to print help text.


[GnuPG]:    https://gnupg.org/
[Graphviz]: https://graphviz.org/


How to Build and Install
------------------------

The programs are written in the Common Lisp language and require [Steel
Bank Common Lisp][SBCL] implementation for compiling. For drawing graphs
the [Graphviz][] tool is required. Both should be available in common
GNU/Linux distributions ([Debian][]: `apt install sbcl graphviz`).

To build the necessary files run this command:

    $ make

To install the files in the default locations run this command:

    $ make install

`make` command will also save some configuration variables to
`config.mk` file. The default installation target is `~/bin` for
executable files. You can configure different install location as well
as `gpg` executable location with Makefile variables and save them to a
configuration file for later runs. For example:

    $ make clean-all
    $ make config bindir=/usr/local/bin gpg=/usr/local/bin/gpg
    $ make
    $ sudo make install

To uninstall all files run command `make uninstall` with the same
configuration as during installation. You can clean build files with
`make clean` or `make clean-all` (which deletes also the configuration).

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
