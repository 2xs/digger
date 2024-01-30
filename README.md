#  Digger

This repository contains a tool to convert Coq code written in a
“C-style” (imperative style based on a monad, with full application of
functions) into the corresponding C code or to an intermediate
representation (deep) output as Coq source code. It starts from the
Coq code extracted as JSON by the internal extraction facility.


The source code is copyright Université de Lille & Veïs Oudjail and
covered by CeCILL-A licence, see `LICENSE`.

The development team is:

*   Samuel Hym
*   Veïs Oudjail


The directory `example/` contains the source code for a small example
of its use. This example works (at least) with Coq 8.9.
