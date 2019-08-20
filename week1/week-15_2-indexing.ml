(* week-15_2-indexing.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2018-2019 *)
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

(* ********** *)

let fold_right_list nil_case cons_case vs =
 (* fold_right_list : 'a -> ('b -> 'a -> 'a) -> 'b list -> 'a *)
  let rec visit vs =
       (* visit : 'b list -> 'a *)
    match vs with
    | [] ->
       nil_case
    | v :: vs' ->
       cons_case v (visit vs')
  in visit vs;;

let fold_left_list nil_case cons_case vs_init =
 (* fold_left_list : 'a -> ('b -> 'a -> 'a) -> 'b list -> 'a *)
  let rec loop vs a =
       (* loop : 'b list -> 'a *)
    match vs with
    | [] ->
       a
    | v :: vs' ->
       loop vs' (cons_case v a)
  in loop vs_init nil_case;;

let fold_right_nat zero_case succ_case n =
 (* fold_right_nat : 'a -> ('a -> 'a) -> int -> 'a *)
  let () = assert (n >= 0) in
  let rec visit i =
    if i = 0
    then zero_case
    else succ_case (visit (pred i))
  in visit n;;

let fold_left_nat zero_case succ_case n =
 (* fold_right_nat : 'a -> ('a -> 'a) -> int -> 'a *)
  let () = assert (n >= 0) in
  let rec loop i a =
    if i = 0
    then a
    else loop (pred i) (succ_case a)
  in loop n zero_case;;

(* ********** *)

type 'a llist =
  | Lnil
  | Lcons of 'a * 'a llist Lazy.t;;

let fold_right_llist lnil_case lcons_case lvs_init =
 (* fold_right_llist : 'a -> ('b -> 'a -> 'a) -> 'b llist -> 'a *)
  let rec visit lvs =
       (* visit : 'b llist -> 'a *)
    match lvs with
    | Lnil ->
       lnil_case
    | Lcons (v, dlvs') ->
       let ih = visit (Lazy.force dlvs')
       in lcons_case v ih
  in visit lvs_init;;

let fold_left_llist lnil_case lcons_case lvs_init =
 (* fold_left_llist : 'a -> ('b -> 'a -> 'a) -> 'b llist -> 'a *)
  let rec loop lvs a =
    (* loop : 'b llist -> 'a *)
    match lvs with
    | Lnil ->
       a
    | Lcons (v, dlvs') ->
       loop (Lazy.force dlvs') (lcons_case v a)
  in loop lvs_init lnil_case;;

(* ********** *)

type 'a binary_tree =
  | Leaf of 'a
  | Node of 'a binary_tree * 'a binary_tree;;

let fold_right_binary_tree leaf_case node_case t_init =
  let rec visit t =
    match t with
    | Leaf v ->
       leaf_case v
    | Node (t1, t2) ->
       node_case (visit t1, visit t2)
  in visit t_init;;

(* ********** *)

module Indexing_strings =
struct

  let test_index_string_left_to_right candidate =
    let b0 = (candidate "0123" 0 = Some '0')
    and b1 = (candidate "0123" 1 = Some '1')
    and b2 = (candidate "0123" 2 = Some '2')
    and b3 = (candidate "0123" 3 = Some '3')
    and b4 = (candidate "0123" 4 = None)
    and b5 = (candidate "0123" ~-1 = None)
    in b0 && b1 && b2 && b3 && b4 && b5;;

  (* ********** *)

  let test_index_string_right_to_left candidate =
    let b0 = (candidate "3210" 0 = Some '0')
    and b1 = (candidate "3210" 1 = Some '1')
    and b2 = (candidate "3210" 2 = Some '2')
    and b3 = (candidate "3210" 3 = Some '3')
    and b4 = (candidate "3210" 4 = None)
    and b5 = (candidate "3210" ~-1 = None)
    in b0 && b1 && b2 && b3 && b4 && b5;;

end;;

(* ********** *)

module Indexing_arrays =
struct

  let test_index_array_left_to_right_int candidate =
    let b0 = (candidate [|0; 1; 2; 3|] 0 = Some 0)
    and b1 = (candidate [|0; 1; 2; 3|] 1 = Some 1)
    and b2 = (candidate [|0; 1; 2; 3|] 2 = Some 2)
    and b3 = (candidate [|0; 1; 2; 3|] 3 = Some 3)
    and b4 = (candidate [|0; 1; 2; 3|] 4 = None)
    and b5 = (candidate [|0; 1; 2; 3|] ~-1 = None)
    in b0 && b1 && b2 && b3 && b4 && b5;;

  (* ********** *)

  let test_index_array_right_to_left_int candidate =
    let b0 = (candidate [|3; 2; 1; 0|] 0 = Some 0)
    and b1 = (candidate [|3; 2; 1; 0|] 1 = Some 1)
    and b2 = (candidate [|3; 2; 1; 0|] 2 = Some 2)
    and b3 = (candidate [|3; 2; 1; 0|] 3 = Some 3)
    and b4 = (candidate [|3; 2; 1; 0|] 4 = None)
    and b5 = (candidate [|3; 2; 1; 0|] ~-1 = None)
    in b0 && b1 && b2 && b3 && b4 && b5;;

end;;

(* ********** *)

module Indexing_lists =
struct

  let test_index_list_left_to_right_int candidate =
    let b0 = (candidate [0; 1; 2; 3] 0 = Some 0)
    and b1 = (candidate [0; 1; 2; 3] 1 = Some 1)
    and b2 = (candidate [0; 1; 2; 3] 2 = Some 2)
    and b3 = (candidate [0; 1; 2; 3] 3 = Some 3)
    and b4 = (candidate [0; 1; 2; 3] 4 = None)
    and b5 = (candidate [0; 1; 2; 3] ~-1 = None)
    in b0 && b1 && b2 && b3 && b4 && b5;;
  
  (* ********** *)
  
  let test_index_list_right_to_left_int candidate =
    let b0 = (candidate [3; 2; 1; 0] 0 = Some 0)
    and b1 = (candidate [3; 2; 1; 0] 1 = Some 1)
    and b2 = (candidate [3; 2; 1; 0] 2 = Some 2)
    and b3 = (candidate [3; 2; 1; 0] 3 = Some 3)
    and b4 = (candidate [3; 2; 1; 0] 4 = None)
    and b5 = (candidate [3; 2; 1; 0] ~-1 = None)
    in b0 && b1 && b2 && b3 && b4 && b5;;

end;;

(* ********** *)

(* The goal of this section is to index a list with a function of type 
 * 'a list -> int -> 'a option. *)

(* 1. Indexing lists from left to right *)

(* a. implement a solution that uses List.nth and List.length *)
let index_list_left_to_right_lib (xs : 'a list) (index : int) : 'a option =
  if index >= 0 &&  index < List.length xs 
  then Some (List.nth xs index)
  else None
;;
  
let _ = assert (Indexing_lists.test_index_list_left_to_right_int
                  index_list_left_to_right_lib);;

(* b. implement a solution that is structurally recursive on the list *)
let rec index_list_left_to_right_rec_on_list
          (xs : 'a list) (index : int) : 'a option =
  match xs with
  | []       ->
     None
  | x :: xs' ->
     if index = 0
     then Some x
     else index_list_left_to_right_rec_on_list xs' (index - 1)
;;

let _ = assert (Indexing_lists.test_index_list_left_to_right_int
                  index_list_left_to_right_rec_on_list);;

(* c. express your solution (for b.) with fold_right_list *)
  
(* One possible solution that reduces the list *) 
let rec index_list_left_to_right_foldr_on_list
          (xs_init : 'a list) (index : int) : 'a option =
  let (reduced_xs, _) = 
    fold_right_list (xs_init, 0)
                    (fun _ (xs, count) ->
                      if count = index
                      then (xs, count)
                      else match xs with
                           | []       -> ([], count + 1)
                           | _ :: xs' -> (xs', count + 1))
                    xs_init
  in match reduced_xs with
     | []       -> None
     | x :: xs' -> Some x
;;

let _ = assert (Indexing_lists.test_index_list_left_to_right_int
                  index_list_left_to_right_foldr_on_list_alt2) ;;

(* We note that it is more natural to use fold_left_list for this computation; the
 * solution involving it is as follows: *)
let rec index_list_left_to_right_foldl_on_list
          (xs : 'a list) (index : int) : 'a option =
  let (opt_val, _) = 
    fold_left_list (None, 0)
                   (fun x ((opt_val, count) as acc) ->
                     match opt_val with
                     | Some _ -> acc
                     | None   ->
                        if count = index
                        then (Some x, index)
                        else (None, count + 1))
                   xs
  in opt_val
;;

let _ = assert (Indexing_lists.test_index_list_left_to_right_int
                  index_list_left_to_right_foldr_on_list) ;;

(* An alternative solution that makes use of List.length to flip the index *) 
let rec index_list_left_to_right_foldr_on_list_alt
          (xs : 'a list) (index : int) : 'a option =
  let (opt_val, _) = 
    fold_right_list (None, List.length xs - 1)
                    (fun x ((opt_val, count) as acc) ->
                      if count < index
                      then acc
                      else if count = index
                      then (Some x, -1)
                      else (None, count - 1)
                    )
                    xs
  in opt_val
;;

let _ = assert (Indexing_lists.test_index_list_left_to_right_int
                  index_list_left_to_right_foldr_on_list_alt) ;;

(* d. implement a solution that is structurally recursive on the index *)
let rec index_list_left_to_right_rec_on_index
          (xs : 'a list) (index : int) : 'a option =
  if index = 0
  then match xs with
       | []      ->
          None
       | x :: xs' ->
          Some x
  else match xs with
       | [] ->
          None
       | x :: xs' ->
          index_list_left_to_right_rec_on_index xs' (index - 1)
;;

let _ = assert (Indexing_lists.test_index_list_left_to_right_int
                  index_list_left_to_right_rec_on_index);;

(* e. express your solution (for d.) with fold_right_nat *)
let rec index_list_left_to_right_foldr_on_nat
          (xs_init : 'a list) (index : int) : 'a option =
  if index < 0
  then None
  else 
    let xs_reduced = 
      fold_right_nat xs_init
                     (fun xs ->
                       match xs with
                       | []       -> []
                       | x :: xs' -> xs')
                     index
    in match xs_reduced with
       | []       -> None 
       | x :: xs' -> Some x
;;

let _ = assert (Indexing_lists.test_index_list_left_to_right_int
                  index_list_left_to_right_foldr_on_nat);;
  
(* 2. Indexing lists from right to left *)

(* a. implement a solution that uses List.nth and List.length *)
let index_list_right_to_left_lib (xs : 'a list) (index : int) : 'a option =
  if index >= 0 && index < List.length xs
  then Some (List.nth xs (List.length xs - 1 - index))
  else None
;;

let _ = assert (Indexing_lists.test_index_list_right_to_left_int
                  index_list_right_to_left_lib);;

(* b. implement a solution that uses List.rev *)
let index_list_right_to_left_rev (xs : 'a list) (index : int) : 'a option =
  index_list_left_to_right_lib (List.rev xs) index
;;

let _ = assert (Indexing_lists.test_index_list_right_to_left_int
                  index_list_right_to_left_rev)
;;

(* c. implement a solution that is structurally recursive on the list *)
let index_list_right_to_left_rec_on_list
      (xs_init : 'a list) (index : int) : 'a option =
  let rec helper xs = 
    match xs with
    | []       ->
       (None, 0)
    | x :: xs' ->
       let ((opt_rv, count) as return_pair) = helper xs'
       in match opt_rv with
          | Some _ ->
             return_pair
          | None   ->
             if count = index
             then (Some x, count + 1)
             else (None, count + 1)
  in
  let (opt_rv, _) = helper xs_init
  in opt_rv
;;

let _ = assert (Indexing_lists.test_index_list_right_to_left_int
                  index_list_right_to_left_rec_on_list)
;;
       
(* the most obvious solution: flip the expected index using List.length *) 
let index_list_right_to_left_rec_on_list_alt
      (xs : 'a list) (index : int) : 'a option =
  index_list_left_to_right_rec_on_list xs (List.length xs - 1 -index)
;;

let _ = assert (Indexing_lists.test_index_list_right_to_left_int
                  index_list_right_to_left_rec_on_list_alt)
;;

(* d. express your solution(s) (for c.) with fold_right_list *)
let index_list_right_to_left_foldr_list (xs : 'a list) (index : int) : 'a option =
  let (opt_val, _) =
    fold_right_list (None, 0)
                    (fun x (op, count) ->
                      if count = index
                      then (Some x, count + 1)
                      else (op, count + 1)) 
                    xs
  in opt_val
;;

let _ = assert (Indexing_lists.test_index_list_right_to_left_int
                  index_list_right_to_left_foldr_list) 
;;
  
(* ********** *)  

module Indexing_lazy_lists =
struct

  let test_index_llist_left_to_right_int candidate =
    let b0 = (candidate (Lcons (0, lazy (Lcons (1, lazy (Lcons (2, lazy (Lcons (3, lazy Lnil)))))))) 0 = Some 0)
    and b1 = (candidate (Lcons (0, lazy (Lcons (1, lazy (Lcons (2, lazy (Lcons (3, lazy Lnil)))))))) 1 = Some 1)
    and b2 = (candidate (Lcons (0, lazy (Lcons (1, lazy (Lcons (2, lazy (Lcons (3, lazy Lnil)))))))) 2 = Some 2)
    and b3 = (candidate (Lcons (0, lazy (Lcons (1, lazy (Lcons (2, lazy (Lcons (3, lazy Lnil)))))))) 3 = Some 3)
    and b4 = (candidate (Lcons (0, lazy (Lcons (1, lazy (Lcons (2, lazy (Lcons (3, lazy Lnil)))))))) 4 = None)
    and b5 = (candidate (Lcons (0, lazy (Lcons (1, lazy (Lcons (2, lazy (Lcons (3, lazy Lnil)))))))) ~-1 = None)
    in b0 && b1 && b2 && b3 && b4 && b5;;
  
  (* ********** *)
  
  let test_index_llist_right_to_left_int candidate =
    let b0 = (candidate (Lcons (3, lazy (Lcons (2, lazy (Lcons (1, lazy (Lcons (0, lazy Lnil)))))))) 0 = Some 0)
    and b1 = (candidate (Lcons (3, lazy (Lcons (2, lazy (Lcons (1, lazy (Lcons (0, lazy Lnil)))))))) 1 = Some 1)
    and b2 = (candidate (Lcons (3, lazy (Lcons (2, lazy (Lcons (1, lazy (Lcons (0, lazy Lnil)))))))) 2 = Some 2)
    and b3 = (candidate (Lcons (3, lazy (Lcons (2, lazy (Lcons (1, lazy (Lcons (0, lazy Lnil)))))))) 3 = Some 3)
    and b4 = (candidate (Lcons (3, lazy (Lcons (2, lazy (Lcons (1, lazy (Lcons (0, lazy Lnil)))))))) 4 = None)
    and b5 = (candidate (Lcons (3, lazy (Lcons (2, lazy (Lcons (1, lazy (Lcons (0, lazy Lnil)))))))) ~-1 = None)
    in b0 && b1 && b2 && b3 && b4 && b5;;

end;;

(* ********** *)

module Indexing_streams =
struct

end;;

(* ********** *)

module Indexing_binary_trees =
struct

end;;

(* ********** *)

(* end of week-15_2-indexing.ml *)

"week-15_2-indexing.ml"
