# lzo-hs
This bundles [lzo](http://www.oberhumer.com/opensource/lzo).

Inspired by the [lzo](https://hackage.haskell.org/package/lzo) package, which
bundled only minilzo. I was hungry for extra compression, which is only provided
by the full library.

Likely only the things I'm using are exposed. I'll gladly accept PRs, and
probably sort any easy issues if you'd like to throw one my way.

## License
This package includes parts of the LZO library. These are licensed under the
GPLv2 (see `LICENSE-LZO-C`).

Uses code from Vanessa McHale's lzo library (see `LICENSE-LZO-HS`).

Original code (under `src`) is licensed under MIT (see `LICENSE`).
