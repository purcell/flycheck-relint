[![Melpa Status](http://melpa.org/packages/flycheck-relint-badge.svg)](http://melpa.org/#/flycheck-relint)
[![Melpa Stable Status](http://stable.melpa.org/packages/flycheck-relint-badge.svg)](http://stable.melpa.org/#/flycheck-relint)
[![Build Status](https://github.com/purcell/flycheck-relint/actions/workflows/test.yml/badge.svg)](https://github.com/purcell/flycheck-relint/actions/workflows/test.yml)
<a href="https://www.patreon.com/sanityinc"><img alt="Support me" src="https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg"></a>

flycheck-relint
===============

This library provides a Flycheck checker for `rx` and string regular
expressions in Emacs Lisp files, using
[relint](https://elpa.gnu.org/packages/relint.html).

Use relint 1.15 - 1.24, this is not yet compatible with 2.0

Installation
------------

The recommended way to get `flycheck-relint` is as a package from the
[MELPA](https://melpa.org) repository. The version of `flycheck-relint` there will
always be up-to-date. There may also be packages in [MELPA
Stable](https://stable.melpa.org), which track the latest numbered tag.

To register the checker, use something like this in your emacs startup file:

```el
(with-eval-after-load 'flycheck
  (flycheck-relint-setup))
```

For use-package:

```el
(use-package flycheck-relint
  :after (flycheck relint)
  :config (flycheck-relint-setup))
```

<hr>

[💝 Support this project and my other Open Source work](https://www.patreon.com/sanityinc)

[💼 LinkedIn profile](https://uk.linkedin.com/in/stevepurcell)

[✍ sanityinc.com](http://www.sanityinc.com/)

[🐦 @sanityinc](https://twitter.com/sanityinc)
