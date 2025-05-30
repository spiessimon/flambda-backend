(* TEST
 arch_amd64;
 flags = "-extension-universe stable";
 include stdlib_stable;
 {
   bytecode;
 }{
   native;
 }
*)

(* CR layouts: this test only runs on AMD64 because ARM64
   does not support float32#. Split off the non-float32#
   parts into a separate test that runs everywhere. *)

open Stdlib_stable

(* Test that [Iarray] is exported. *)
let () =
  let arr = Iarray.init 4 (fun x -> x) in
  assert (Iarray.get arr 2 = 2)
;;

(* Test that [IarrayLabels] is exported. *)
let () =
  let arr = IarrayLabels.init 4 ~f:(fun x -> x) in
  assert (IarrayLabels.get arr 2 = 2)
;;

(* Test that [Float32] is exported. *)

let () =
  let one = Float32.of_float 1.0 in
  assert (Float32.to_float one = 1.0)
;;

(* Test that [Float32_u] is exported. *)

let () =
  let one = Float32_u.of_int 1 in
  assert (Float32_u.to_int one = 1)
;;

(* Test that [Or_null] is exported. *)

let () =
  match Or_null.null with
  | Null -> ()
  | This _ -> assert false
;;
