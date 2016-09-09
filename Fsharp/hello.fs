printfn "Hello World"

(*val a : int = 3*)

(*
let a = 3

type mood = Happy | Sad | Mad

let f x = 
    match x with
    |   Happy -> "\n:-)\n"
    |   Sad   -> "\n:-(\n"
    |   _     -> "\n:-@\n"

System.Console.WriteLine(f Happy)

type btree = Empty
            | Node of (int * btree * btree)

let e = Empty
let t3 = Node(3,e,e)
let t5 = Node(5,e,e)
let t9 = Node(9, t3, t5)

let newTree n = Node(n, Empty, Empty)

let rootValue t = 
    match t with
    | Node (n, t1, t2) -> n
    | Empty            -> failwith "Not empty!"


//let Node(n, s1, s2) = t4

let rec occurs n t =
    match t with
    | Empty -> false
    | Node (m, t1, t2) -> (m = n) || (occurs n t1) || (occurs n t2)

let rec insert n t = 
    match t with
        Empty -> newTree n
        | Node (m, t1, t2) ->
            if (n < m) then
                Node (m, insert n t1, t2)
            else
                Node (m, t1, insert n t2)

occurs 10 t3
*)

type 'a list = E | L of 'a * 'a list

let l1 = L(1, L(2, E))

let l2 = L("a", L("b", E))

let head l = 
    match l with 
    | L (h, _) -> h
    | E -> failwith "list is empty"

let tail l = 
    match l with 
    | L (_, t) -> t
    | E -> failwith "list is empty"

let rec last l = 
    match l with
    | L (h, E) -> h
    | L (h, t) -> last t
    | E -> failwith "list is empty"

let rec length l =
    match l with
    | E -> 0
    | L(_, t) -> 1 + length t

(*
let rec print_list l =
    match l with
    | E -> printfn "[]"
    | L (h, t) -> Console.WriteLine(h)
    *)

//print_list l1

printfn "%A" l1
(*
List how to:

[] : empty list
[3;5;6;3]

[(1,2); (1,4)]

[(3,1)]; []; [4;5;6]

6 :: [] // construct list

*)


