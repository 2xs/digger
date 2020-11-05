(* This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software.  You can  use,
 * modify and/ or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".

 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights,  and the successive licensors  have only  limited
 * liability.

 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or
 * data to be ensured and,  more generally, to use and operate it in the
 * same conditions as regards security.

 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 *)

(* This Coq module uses the monad defined in the Monad module to
   describe sequential computations. *)

Require Import Monad Internals.
Open Scope monad_scope.

Definition id (b : bool) : M bool := returnM b.

Definition neg (b : bool) : M bool := if b then returnM false else returnM true.

Fixpoint prepare recBound : M unit :=
  match recBound with
  | O   => emptyM
  | S b => do r <- ready ;
              if r
              then returnM tt
              else do _ <- getReady ;
                      prepare b
  end.

Definition fullPrepare := prepare bound.

Eval compute in runM (id true) 12.
Eval compute in runM (neg true) 12.
Eval compute in runM (prepare 1) 1.
Eval compute in runM (prepare 2) 1.