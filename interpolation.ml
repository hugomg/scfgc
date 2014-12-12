open Core.Std

type 'a t = I of string * (int * int * 'a) list


let map ~f (I(str, xs)) =
  I(str, List.map xs ~f:(fun (i,j,x) -> (i,j,f x)))


(* The ocaml standard library doesn't have a global-substitute
 * function for regexes and I'd rather avoid requiring dependency
 * just for this... *)
let of_string str =

  let out = Queue.create () in

  let isid x =
    Char.is_alphanum x || x = '_'
  in

  let rec sA i j =
    if j < String.length str && str.[j] <> '$' then
      sA i (j+1)
    else (
      if j < String.length str then
        sB j (j+1)
    )
  and sB i j =
    if j < String.length str && isid str.[j] then
      sB i (j+1)
    else (
      Queue.enqueue out (i, j, String.slice str (i+1) j);
      if j < String.length str then
        sA j j
    )
  in

  sA 0 0;

  I(str, Queue.to_list out)


let to_string (I(str, xs)) =

  let out = Buffer.create (2*String.length str) in

  let j = List.fold_left xs ~init:0 ~f:(fun i (j,k,s) ->
    Buffer.add_substring out str i (j-i);
    Buffer.add_string out s;
    k
  ) in
  Buffer.add_substring out str j (String.length str - j);

  Buffer.contents out
