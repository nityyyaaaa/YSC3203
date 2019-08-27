(* week-2_TABA_HW.ml *)
(* Advanced Data Structure and Algorithms (YSC 3203), Sem1, 2019-2020 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of 23 Apr 2019 *)

(* ********** *)

(*
   name: Oishik Ganguly 
   student ID number: A0138306J
   e-mail address: oishik.ganguly@u.yale-nus.edu.sg
*)

(*
   name: Nitya Sriram
   student ID number:A0156281E
   e-mail address: nityasriram@u.yale-nus.edu.sg
*)

(* ********** Convolutions ********** *)

let test_cnv1 candidate =
  let b0 = (candidate [] [] = [])
    and b1 = (candidate [1] [1] = [(1,1)])
    and b2 = (candidate [1;2] [1;2] = [(1,2);(2,1)])
    and b3 = (candidate [1;2;3] [1;2;3] = [(1,3);(2,2);(3,1)])
    in b0 && b1 && b2 && b3;;
  

let cnv1 xs ys =
  let rec continue a y ans =
  match a,y with
  | ah::at, y::ys -> continue at ys ((ah,y)::ans)
  | [], [] -> ans
    in
 let rec walk x a =
    match x with
    | [] -> continue a ys []
    | xh::xt -> walk xt (xh::a)
 in walk xs [];;

let () = assert (test_cnv1 cnv1);;


(* ********** *)

(* ********** List Reversal ********** *)

(* ********** *)

(* ********** (Nitya) ********** *)

(* ********** *)

(* ********** (Oishik) ********** *)

(* ********** *)


