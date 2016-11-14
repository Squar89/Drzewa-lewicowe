(*Autor - Jakub Wróblewski 38401
Recenzent - Jakub Obuchowski (*wpisz swój numer*)*)
type 'a queue =
| Leaf
| Node of 'a queue * 'a * int * 'a queue
;;

let empty = Leaf
;;

exception Empty
;;

let is_empty q =
    match q with
    | Leaf -> true
    | _ -> false
;;

(*funkcja tworząca nową, jednoelementową kolejkę*)
let create x = Node (Leaf, x, 1, Leaf)
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
    | Node (ql, x, h, qp) ->
        if(height ql < height qp) then
            Node(qp, x, ((height ql) +1), ql)
        else
            Node(ql, x, ((height qp) +1), qp)
;;

(*funkcja łącząca dwie kolejki*)
let rec join q1 q2 =
    match q1, q2 with
    | Node(q1l, x1, h1, q1p), Node(q2l, x2, h2, q2p) ->
        if(x1 > x2) then
            join q2 q1
        else
            let r = join q1p q2 in
            check (Node(q1l, x1, h1, r))
    | Node(q1l, x1, h1, q1p), Leaf ->
        Node(q1l, x1, h1, q1p)
    | Leaf, Node(q2l, x2, h2, q2p) ->
        Node(q2l, x2, h2, q2p)
    | Leaf, Leaf ->
        Leaf
;;

let add e q =
    join (create e) q
;;

let delete_min q =
    match q with
    | Leaf ->
        raise Empty
    | Node (ql, x, h, qp) ->
        (x, join ql qp)
        
;;