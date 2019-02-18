GPG Tofu
=========

**Display "trust on first use" (TOFU) statistics of a GnuPG keys**


Introduction
------------

`gpg-tofu` is a command-line tool which inputs public key data from
`gpg` ([GnuPG][]) security tool and outputs "trust on first use" (TOFU)
statistics about the keys.

The program is written in the Common Lisp language and requires [Steel
Bank Common Lisp][SBCL] implementation. It should be available in common
GNU/Linux distributions (Debian: `apt install sbcl`).

[GnuPG]:    https://gnupg.org/
[SBCL]:     http://sbcl.org/


Examples
--------

Usage: `gpg-tofu [key1 ...]`

    $ gpg-tofu tlikonen@iki.fi

    pub 4E1055DC84E9DFF613D78557719D69D324539450
    uid [ultimate] Teemu Likonen <tlikonen@iki.fi>
        TOFU validity: (4/4) a lot of history for trust, TOFU policy: good
        430 signatures in 1 year 254 days, first: 2017-06-09 11:28:16Z, last: 2019-02-18 14:18:37Z
        404 encryptions in 1 year 244 days, first: 2017-06-15 14:41:30Z, last: 2019-02-14 19:25:41Z
    [...]


Author and license
------------------

Author: Teemu Likonen <<tlikonen@iki.fi>>

PGP: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

No restrictions for use: this program is placed in the public domain.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
