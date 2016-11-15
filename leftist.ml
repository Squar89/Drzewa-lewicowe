(*Autor - Jakub Wróblewski 38401
Recenzent - Jakub Obuchowski grupa 4 (LAB) 385877*)
type 'a queue =
| Leaf
| Node of 'a queue * 'a * int * 'a queue
;;
(*Typ składający się z:
lewe poddrzewo|priorytet|prawa wysokość|prawe poddrzewo*)

let empty = Leaf
;;

exception Empty
;;

let is_empty q =
    match q with
    | Leaf -> true
    | _ -> false
;;

(*funkcja zwracająca wysokość drzewa*)
let height q =
    match q with
    | Leaf -> 0
    | Node(_, _, h, _) -> h
;;

(*funkcja sprawdzająca lewicowość drzewa*)
let check q =
    match q with
    | Leaf -> Leaf
    | Node (ql, x, _, qr) ->
        if(height ql < height qr) then
            Node(qr, x, ((height ql) +1), ql)
        else
            Node(ql, x, ((height qr) +1), qr)
;;

(*funkcja łącząca dwie kolejki*)
let rec join q1 q2 =
    match q1, q2 with
    | Node(q1l, x1, h1, q1r), Node(_, x2, _, _) ->
        if(x1 > x2) then
            join q2 q1
        else
            let j = join q1r q2 in
            check (Node(q1l, x1, h1, j))
    | t1, Leaf -> t1
    | Leaf, t2 -> t2
;;

let add e q =
    let create x = Node (Leaf, x, 1, Leaf) in
    join (create e) q
;;

let delete_min q =
    match q with
    | Leaf ->
        raise Empty
    | Node (ql, x, _, qr) -> 
        (x, join ql qr)
        
;;

(*

open Leftist;;

let a = empty;;
assert(is_empty a = true);;
let a = add 5 a;;
assert(is_empty a = false);;
let a = add 6 a;;
let a = add 1 a;;
let a = add 29 a;;
let a = add 4 a;;

let b = empty;;
let b = add 9 b;;
let b = add 5 b;;
let b = add 3 b;;
let b = add 8 b;;
let b = add 30 b;;
let b = add 15 b;;

let c = a;; (* [1, 4, 5, 6, 29] *)
let d = b;; (* [3, 5, 8, 9, 15, 30] *)

let e = join a b;; (* [1, 3, 4, 5, 5, 6, 8, 9, 15, 29, 30] *)

assert(fst (delete_min c) = 1);;
let c = snd (delete_min c);;
assert(fst (delete_min c) = 4);;
let c = snd (delete_min c);;
assert(fst (delete_min c) = 5);;
let c = snd (delete_min c);;
assert(fst (delete_min c) = 6);;
let c = snd (delete_min c);;
assert(fst (delete_min c) = 29);;
let c = snd (delete_min c);;

assert(fst (delete_min d) = 3);;
let d = snd (delete_min d);;
assert(fst (delete_min d) = 5);;
let d = snd (delete_min d);;
assert(fst (delete_min d) = 8);;
let d = snd (delete_min d);;
assert(fst (delete_min d) = 9);;
let d = snd (delete_min d);;
assert(fst (delete_min d) = 15);;
let d = snd (delete_min d);;
assert(fst (delete_min d) = 30);;
let d = snd (delete_min d);;

assert(fst (delete_min e) = 1);;
let e = snd (delete_min e);;
assert(fst (delete_min e) = 3);;
let e = snd (delete_min e);;
assert(fst (delete_min e) = 4);;
let e = snd (delete_min e);;
assert(fst (delete_min e) = 5);;
let e = snd (delete_min e);;
assert(fst (delete_min e) = 5);;
let e = snd (delete_min e);;
assert(fst (delete_min e) = 6);;
let e = snd (delete_min e);;
assert(fst (delete_min e) = 8);;
let e = snd (delete_min e);;
assert(fst (delete_min e) = 9);;
let e = snd (delete_min e);;
assert(fst (delete_min e) = 15);;
let e = snd (delete_min e);;
assert(fst (delete_min e) = 29);;
let e = snd (delete_min e);;
assert(fst (delete_min e) = 30);;
let e = snd (delete_min e);;

"Sprawdzenie, czy Empty dziala:"
let test_empty1 = 
  try 
    let _ = (delete_min c) in "test nie dziala"
  with 
    | Empty -> "ok (1)";;
    
let test_empty1 = 
  try 
    let _ = (delete_min d) in "test2 nie dziala"
  with 
    | Empty -> "ok (2)";;
    
let test_empty1 = 
  try 
    let _ = (delete_min e) in "test3 nie dziala"
  with 
    | Empty -> "ok (3)";;

*)