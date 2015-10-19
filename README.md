##tct-jbc
This package is part of the _Tyrolean Complexity Tool (TcT)_ and provides
automatic complexity analysis of _Jinja Bytecode (JBC)_.

This repository provides the `tct-jbc` library as well as the `tct-jbc` executable.

##Requirements

Executables:
  * [Glasgow Haskell Compiler, version 7.10](http://www.haskell.org/ghc/) 
  * [minismt, version 0.6](http://cl-informatik.uibk.ac.at/software/minismt/) or [z3, version 4.3](https://github.com/Z3Prover/z3)
  * [yices, version 2.3](http://yices.csl.sri.com/)

Other packages
  * [slogic](https://github.com/ComputationWithBoundedResources/slogic/)
  * [jat](https://github.com/ComputationWithBoundedResources/jat/)
  * [tct-core](https://github.com/ComputationWithBoundedResources/tct-core/)
  * [tct-common](https://github.com/ComputationWithBoundedResources/tct-common/)
  * [tct-its](https://github.com/ComputationWithBoundedResources/tct-trs/)
  * [tct-trs](https://github.com/ComputationWithBoundedResources/tct-its/)

The tool is only tested under GNU/Linux.

###Installation

####Using Stack
We recommend using [stack](https://github.com/commercialhaskell/stack) with the accompanied `stack.yaml` file.
To build and install the package run following command:

```bash
stack install tct-jbc
```

###Example Usage
The installation provides an executable `tct-jbc`.

```bash
tct-jbc examples/minsort.raml.trs
```

For full options, run `tct-jbc --help`.

