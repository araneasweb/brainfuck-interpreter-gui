# brainfuck-interpreter-gui

## A Brainfuck Text Editor, Interpreter, and Turing Machine Simulator

An integrated development environment for the esolang brainfuck. It includes a text editor with syntax highlighting, an interpreter, debugging with breakpoints/stepping, and turing machine simulator.

## Acknowledgments

We surely built on the work of others! Here are resources and people we got support from:

+ [Codeblocks from the Brainfuck Wikipedia page](https://en.wikipedia.org/wiki/Brainfuck) for tests and language understanding.
+ [kavehmz/brainfuck_examples](https://github.com/kavehmz/brainfuck_examples/tree/master) for tests.
+ [haskell-gi](https://github.com/haskell-gi) for a GUI. (well, bindings to a gui library more specifically)

## How to test and run the MVP

**Ensure you have the [nix package manager](https://nixos.org/download/) and the installed or the code will not compile!**

(For Debian based systems (including WSL2), must run ``sudo apt install nix-bin`` first.)

To test the interpreter functionality, simply run `make` from the root directory or `make test` from `./haskell`.

To use the interpreter, simply run the command `stack exec brainfuck-interpreter-gui-exe -- -f <<PATH TO FILE>>` (without the `<<>>`) to open a `.bf` file or `stack exec brainfuck-interpreter-gui-exe -- -s STRING` to directly interpret a brainfuck string. (Technically you could run this through ghci, but it returns a `tape`, which is an infinite piece of data so you probably don't want to `show` that.)

To run the GUI, enter `./haskell` and run the command `stack exec brainfuck-interpreter-gui-exe -- -g` or run the command `make gui`. You can also run `stack ghci`/`make ghci`, then run `:l GUITest` followed by `run`.

### Dependencies

|Haskell Libraries|Packages (via Nix)|
|-|-|
|base|glib|
|binary|gobject-introspection|
|containers|pcre2|
|silently|freetype|
|gi-gtk|cairo|
|freetype2|expat|
|cairo|fontconfig|
|haskell-gi-base|util-linux|
|text|harfbuzz|
|mtl|libselinux|
|gi-glib|libsepol|
|gi-gdk|pkg-config|
|gi-gobject|xorg.libXdmcp|
||gtk3|
||lerc|
||libthai|
||libdatrie|
||libxkbcommon|
||libepoxy|
||xorg.libXtst|
||libsysprof-capture|

## License

This project is licensed under the [GNU General Public License v3.0](./LICENSE).
