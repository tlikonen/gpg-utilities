GPG-tofu
=========

**Display trust on first use (TOFU) statistics of a GnuPG keys**


Introduction
------------

`gpg-tofu` is a command-line tool which inputs public key data from
`gpg` ([GnuPG][]) security tool and outputs trust on first use (TOFU)
statistics about the keys.

The program is written in the Common Lisp language and requires [Steel
Bank Common Lisp][SBCL] implementation. It should be available in common
GNU/Linux distributions (Debian: `apt install sbcl`).

[GnuPG]:    https://gnupg.org/
[SBCL]:     http://sbcl.org/


Examples
--------

Usage: `gpg-tofu [key1 ...]`

$ gpg-tofu Werner

    80615870F5BAD690333686D0F2AD85AC1E42B367
      [full    ] Werner Koch <wk@gnupg.org>
        TOFU validity: (4/4) a lot of history for trust, TOFU policy: good
        340 signatures in 1 year 251 days, first: 2017-06-09 11:28:33, last: 2019-02-15 19:11:54
      [full    ] Werner Koch <wk@g10code.com>
        TOFU validity: (4/4) a lot of history for trust, TOFU policy: good
        340 signatures in 1 year 251 days, first: 2017-06-09 11:28:33, last: 2019-02-15 19:11:54
      [full    ] Werner Koch <werner@eifzilla.de>
        TOFU validity: (4/4) a lot of history for trust, TOFU policy: good
        340 signatures in 1 year 251 days, first: 2017-06-09 11:28:33, last: 2019-02-15 19:11:54
    D8692123C4065DEA5E0F3AB5249B39D24F25E3B6
      [full    ] Werner Koch (dist sig)
        TOFU validity: (2/4) little history, TOFU policy: good
        1 signature in 2017-10-20 16:28:59


Author and license
------------------

Author: Teemu Likonen <<tlikonen@iki.fi>>

PGP: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

License: Public domain

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
