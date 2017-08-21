open CellMap
open Value
open Coordinates

module SheetMap = Map.Make(Coordinates)

open SheetMap

exception No_Dependencies

let empty = SheetMap.empty

let write time tag cell coords sheet =
  try
    let c = find coords sheet in
    let c' = CellMap.write time tag cell c in
    (add coords c' sheet)
  with Not_found ->
    let cell = CellMap.write time tag cell CellMap.empty in
    (add coords cell sheet)


let write_seq time tag origin length width cells sheet =
  let f (sheet,coord) cell =
    let s =  write time tag cell coord sheet in
    let succ_coord = Coordinates.next origin length width in
    try
      (s, succ_coord coord)
    with No_successor -> (s,coord) in
  fst (List.fold_left f (sheet,origin) cells)

let read time tag coords sheet =
  let c = find coords sheet in
  CellMap.cells time tag c

let read_seq filter disam time tag origin length width sheet =
  let succ_coord = Coordinates.next origin length width in
  let sup = match origin with
      {col=c; row=r} -> coords (c + length - 1) (r + width - 1) in
  let coord_list  =
    let rec aux current_coord coord_list =  
      match current_coord with
        c when c = sup -> c::coord_list
      | c when origin <= c && c <= sup ->
        aux (succ_coord c) (c::coord_list)
      | _ -> assert false in
    aux origin [] in
  let l = List.map
      (fun c ->
         try
           let cell_map = find c sheet  in
           c,CellMap.cell disam time tag cell_map
         with
           Not_found ->
             c,(Cell.cell Definition.none)) coord_list in
  match filter with
    None   -> l
  | Some f -> 
    List.filter f l


let string_of_cell coords sv acc =
  let soi = string_of_int in
  let sc = (function {col=c; row=r} ->
      soi c ^ " " ^  soi r ^ ": ") coords in
  sc ^ sv ^ acc


let filter_coords p s =
  let m = filter p s in
  let b = bindings m in
  fst @@ List.split b

(* let dependencies formula s = match formula with  *)
(*     {ty=TyCount; data=Some [r1;c1;r2;c2;_]} -> *)
(*     let inf = coords r1 c1 in *)
(*     let sup = coords r2 (c2+1) in *)
(*     let p c _ = lteq inf c && lteq c sup in *)
(*     let l = filter_coords p s in *)
(*     l *)
(*   | _ -> raise No_Dependencies *)


(* let dependents disam time tag r0 c0 s = *)
(*   let p _ v = match CellMap.value disam time tag v with *)
(*       {ty=TyCount; data=_} -> true *)
(*     | _ -> false in *)
(*   let m = filter p s in *)
(*   let coords_formulae = List.map (function (coords,cell) -> *)
(*       (coords,CellMap.value disam time tag cell)) (bindings m) in *)
(*   let p' r1 c1 r2 c2 = (r1,c1) <= (r0,c0) && (r0,c0) <= (r2,c2) in *)
(*   let rec aux coords_formulae_list acc = *)
(*     match coords_formulae_list with *)
(*       (c, {ty=TyCount; data=Some [r1;c1;r2;c2;_]})::t *)
(*       when p' r1 c1 r2 c2  -> aux t (c::acc) *)
(*     |[] -> acc *)
(*     |_::t -> aux t acc in *)
(*   let l = aux coords_formulae [] in *)
(*   l *)

let string_of_value_list left right tag_format value_format sep vlist =
  "[" ^
  (
    List.fold_left (
      fun s ((_,t),v) ->
        (left ^ tag_format t ^ value_format v ^ right ^ sep ^ s)
    ) "" vlist) ^  "]"

let string_of_values vlist =
  string_of_value_list
    "" ""
    (fun s -> s ^ ": ")
    string_of_value
    ";" vlist


let string_of_sheet time tag s =
  let f c = (string_of_values @@ CellMap.cells time tag c) ^ " | " in 
  let ssheet = map f s in
  fold string_of_cell ssheet ""

let dimensions sheet =
  try
    let {col=l; row=w} = fst (max_binding sheet) in
    Dimensions.({length=(l+1); width=(w+1)})
  with
  Not_found -> Dimensions.({length=0; width=0})
