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

(* This Coq module defines a simple state monad with failure, as an
   example of use of digger to extract some monadic code into C code *)

(* A simple notion of state *)
Definition state := nat.

Definition M (A: Type) := state -> option (A * state).

Definition runM {A: Type} (x: M A) (s: state) := x s.
Definition returnM {A: Type} (a: A) : M A := fun s => Some (a, s).
Definition emptyM {A: Type} : M A := fun s => None.
Definition bindM {A B: Type} (x: M A) (f: A -> M B) : M B :=
  fun s =>
    match runM x s with
    | None => None
    | Some (x', s') => runM (f x') s'
    end.

Definition get : M state := fun s => Some (s, s).
Definition put (s: state) : M unit := fun s' => Some (tt, s).

Notation "'do' x <- a ; b" :=
  (bindM a (fun x => b))
    (at level 200, x ident, a at level 100, b at level 200)
  : monad_scope.