tct-jbc
=======
This package is part of the _Tyrolean Complexity Tool (TcT)_ and provides
automatic complexity analysis of _Jinja Bytecode (JBC)_.

Requirements
------------
  * [Glasgow Haskell Compiler, version 7.8](http://www.haskell.org/ghc/)
  * [slogic](https://github.com/ComputationWithBoundedResources/slogic/)
  * [tct-core](https://github.com/ComputationWithBoundedResources/tct-core/)
  * [tct-common](https://github.com/ComputationWithBoundedResources/tct-common/)
  * [term-rewriting-xml](https://github.com/ComputationWithBoundedResources/term-rewriting-xml/)
  * [tct-trs](https://github.com/ComputationWithBoundedResources/tct-trs/)
  * [tct-its](https://github.com/ComputationWithBoundedResources/tct-its/)
  * [minismt, version 0.6](http://cl-informatik.uibk.ac.at/software/minismt/)

The tool is only tested under GNU/Linux.

Install
-------
For building, you need [ghc](http://www.haskell.org/ghc/) and
[cabal](http://www.haskell.org/cabal/). Execute following bash commands to
install the packages and executables via `cabal`.

  ```
  mkdir tct-bundle
  cd tct-bundle
  git clone https://github.com/ComputationWithBoundedResources/slogic
  git clone https://github.com/ComputationWithBoundedResources/tct-core
  git clone https://github.com/ComputationWithBoundedResources/tct-common
  git clone https://github.com/ComputationWithBoundedResources/term-rewriting-xml
  git clone https://github.com/ComputationWithBoundedResources/tct-trs
  git clone https://github.com/ComputationWithBoundedResources/tct-its
  git clone https://github.com/ComputationWithBoundedResources/tct-jbc
  cabal install **/*.cabal
  ```

Usage
-----
The installation provides an executable `tct-jbc`. For full options, run
`tct-jbc --help`.

