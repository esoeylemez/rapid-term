Rapid-term
==========

When developing interactive command line applications in an editor like
Emacs GHCi typically has no access to an actual terminal.  This is good
enough for applications that only read lines from stdin and print
diagnostics to stdout, but as soon as terminal functionality is needed,
the application has to be tested elsewhere.

This package provides functionality that, when used together with the
[rapid library][], can open a persistent terminal that the application
can access directly, such that terminal applications can be tested
directly with the main GHCi instance.

[rapid library]: https://hackage.haskell.org/package/rapid
