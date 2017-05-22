open Value
open Coordinates


let value_list_printer left right tag_format value_format sep vlist =
  print_string "[";
  print_string (
    List.fold_left (
      fun s ((_,t),v) ->
        (left ^ tag_format t ^ value_format v ^ right ^ sep ^ s)
    ) "" vlist);
  print_endline "]"

let print_values vlist =
  value_list_printer
    "" ""
    (fun s -> s ^ ": ")
    string_of_value
    ";" vlist

let print_json_values vlist =
  value_list_printer
    "{" "}"
    (fun s -> "tag: " ^ s ^ ", val:" )
    json_string_of_value
    ", " vlist

let print_time t = print_endline @@ "at time :" ^ Nativeint.to_string t




(* Single read/write test
   at coords 0 0 time t with tag Alice in sheet s value 42 *) 
let s = Sheet.empty

let s = Sheet.write (Time.current ()) "Alice" (int 42) (coords 0 0) s

let v = Sheet.read (Time.current ()) "Jack" (coords 0 0) s

let () = print_endline
    "Single read/write test at coords 0 0 time t with tag Alice in sheet s value 42"

let () = print_values v

let () = print_endline ""

(* Multiple same agent writes at the same time.
 At coords 0 0, at time t in sheet s value 56.
 At coords 0 1, at time t in sheet s value =#(0,0,1,1,5). JSON Serialization. *)  

let t = Time.(touch (); current () )

let s = Sheet.write t "Alice" (int 56) (coords 0 0) s

let s = Sheet.write t "Alice" (count 0 0 1 1 5) (coords 0 1) s

let v = Sheet.read t "Alice" (coords 0 1) s


let () = print_endline
    "Multiple same agent writes at the same time. At coords 0 0, at time t in sheet s value 56. At coords 0 1, at time t in sheet s value =#(0,0,1,1,5)"
  
let () = print_endline @@ Sheet.string_of_sheet t "Alice"  s

let () = print_endline ""

let () = print_endline "For JSON serlization of values"

let () = print_json_values v

let () = print_endline ""

(* Sequencial reading and writing of values *)

let c = Cell.empty

let t = Time.(touch (); current ())

let c = Cell.write t "Alice"(int 42) c

let to_json l =
  let sep = ", "
  and lb =  "["
  and rb =  "]" in
  let bracketed s =
    lb ^ s ^ rb in
  let coords c =
    "{coords: " ^ (Coordinates.to_yojson c |> Yojson.Safe.to_string)
  and value v  =
    ", value: " ^ (Value.to_yojson v |> Yojson.Safe.to_string) ^ "}"
  in
  let located_value (c,v) = coords c ^ value v in
  let separated_located_value cv = sep ^ located_value cv in
  let f s cv = s ^ separated_located_value cv in
  match l with
    [] -> bracketed ""
  | [cv] -> bracketed (located_value cv)
  | h::t ->
    let l = List.fold_left f (located_value h) t in
    bracketed l

let l = [(int 56);(int 532);
         (int 2);(int 549);
         (int 56); ((count 1 1 7 2 56));
         (int 83)]

let t = Time.(touch (); current ())

let s = Sheet.write t "Alice" (int 56 ) (coords 1 2) s

let s = Sheet.write t "Alice" (count 1 1 7 2 2) (coords 7 2) s

let s = Sheet.write_seq t "Alice" (coords 1 1) 7 1 l  s

let l = Sheet.read_seq (fun ((_,t),_) -> t="Bob" ) t "Alice" (coords 1 1) 7 2 s

let () = print_endline "Values in JSON"

let () = to_json l |> print_endline

let () = print_endline ""
    
let () = print_endline "Reading of values"

let () =
  let f s (c,v) =
    let soi = string_of_int in
    let soc = (function {col=c; row=r} ->
        soi r ^ " " ^  soi c ^ ": ") in
    let cs = soc c in
    let vs = string_of_value v in
    cs ^ vs ^ " | " ^ s in
  let s = List.fold_left f "" l in
  print_endline s

let () = print_endline ""

(* Independent evaluation *)

let env = Evaluator.Environment.empty

let env = Evaluator.define (fun ((_,t),_) -> t="Bob" ) (coords 6 1)  t "Alice" (count 1 1 7 2 56)  s env

let env = Evaluator.define (fun ((_,t),_) -> t="Bob" ) (coords 7 2)  t "Alice" (count 1 1 7 2 3)   s env

let v   = Evaluator.value (coords 7 2) t "Alice" s env

let () = print_endline "Evaluation of =#(1,1,7,2,3) with previous writes"
    
let () = print_endline @@ string_of_value v

let () = print_endline ""

let v   = Evaluator.value (coords 6 1) t "Alice" s env

let () = print_endline "Evaluation of =#(1,1,7,2,56) with previous writes"
    
let () = print_endline @@ string_of_value v

let () = print_endline ""

(* Depencies for =#(1,1,6,2,5) *)
(* Dependents of 1,1 according to Alice at time t in sheet s *)

let dependencies = Sheet.dependencies (count 1 1 6 2 5) s

let dependents = Sheet.dependents (fun ((_,t),_) -> t="Bob") t "Alice" 1 1 s

let print_coord_list l =
  let f s c =
    let soi = string_of_int in
    let soc = (function {col=c; row=r} ->
        soi r ^ " " ^  soi c) in
    let cs = soc c in
    cs ^ "; " ^ s in
  let s = List.fold_left f "" in
  print_endline (s l)

let () = print_endline "Depencies for =#(1,1,6,2,5)"

let () = print_coord_list dependencies

let () = print_endline ""

let () = print_endline "Dependents of 1,1 according to Alice at time t in sheet s"

let () = print_coord_list dependents

let () = print_endline ""

(*Diff *) 

let c =  Cell.empty

let t = Time.current ()

let () = print_time t

let c = Cell.write t "Alice" (int 42) c

let v = Cell.values t "Alice" c

let () = print_values v

let () = print_endline ""

let () = Time.touch ()

let t' = Time.current ()

let () = print_time t'

let c = Cell.write t' "Alice"  (int 56) c

let c = Cell.write t' "Bob" (int 58) c

let diff = Cell.diff "Trudy" t t' c

let () =
  let f s c =
    let soi = Nativeint.to_string in
    let soc = (function ((time,tag), value) ->
        soi time ^ ", " ^ tag ^ ": " ^ string_of_value value) in
    let cs = soc c in
    cs ^ "; " ^ s  in
  let s = List.fold_left f "" in
  print_endline (s diff)

