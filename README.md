# Borrowed and tinkered Emacs Configuration

This is an emacs config. It started out life as a fork from [Chris
Wellon's](https://nullprogram.com/blog/2011/10/19/) config. But has been
subject to a bit of tinkering since then. The basic approach to building
remains the same, the original is pretty solid in this respect. The content of
the config has changed quite a bit, as my personal needs for emacs were a bit
different.

To install all third-party packages, run `make` in this repository
while connected to the internet:

    make

This will clone additional repositories containing the configured
packages (see `packages.el`) into `gpkg/`, install each package under
`site-lisp/<emacs-version>/`, add each to the `load-path`, and
compile. To install/build packages for another version of Emacs, set
the `EMACS` variable:

    make EMACS=emacs26

Package installations for multiple versions of Emacs can safely
coexist side-by-side. If you did everything right Emacs should simply
launch with no errors. You will be greeted with a featureless, empty
gray box awaiting your instructions.


