open Core.Std

type ('a,'n) t = 
  | Leaf of 'a
  | Test of 'n * ('a, 'n) t array

let rec fold d ~leaf ~test = match d with
  | Leaf(x) -> leaf x
  | Test(v, xs) -> test v (Array.map xs ~f:(fold ~leaf ~test))

let test1 v xs =
  Test(v, Array.map xs ~f:(fun x -> Leaf(x)))

let rec find_exn d ass =
  match d with 
  | Leaf(x) -> x
  | Test(v, xs) ->
      let i = List.Assoc.find_exn ass v in
      find_exn xs.(i) ass

let normalize d =
  let rec go ass d = match d with
    | Leaf _ -> d
    | Test(v, xs) ->
      match List.Assoc.find ass v with
      | None ->
        Test(v, Array.mapi xs ~f:(fun i d'->
          go ((v,i)::ass) d'))
      | Some n ->
        go ass xs.(n)
  in
  go [] d

(* A vastly more efficient version of Decision.all, for when
 * we know that the set of variables in the trees is the same *)
let rec merge_exn ts =
  match ts with
  | [] -> Leaf []
  | (first::rest) ->
    match first with
    | Leaf x ->
        let xs = List.map rest ~f:(function
          | Leaf y -> y
          | _ -> assert false) in
        Leaf (x :: xs)
    | Test(v, xs) ->
        let xss = List.map rest ~f:(function
          | Test(w, ys) when v = w -> ys
          | _ -> assert false ) in
        let cols = List.map (xs::xss) ~f:(Array.to_list) in
        let lines = List.transpose_exn cols in
        let ds = List.map lines ~f:merge_exn in
        Test(v, Array.of_list ds)




let variables ~comparator d = fold d
  ~leaf:(fun _ -> Set.empty ~comparator)
  ~test:(fun v vss ->
    let vss' = Set.union_list ~comparator @@ Array.to_list vss in
    Set.add vss' v)

include Monad.Make2(struct
  type nonrec ('a, 'b) t = ('a, 'b) t
  let return x = Leaf x
  let bind m f = normalize @@ fold m ~leaf:f ~test:(fun v xs -> Test(v,xs))
  let map = `Define_using_bind
end)
