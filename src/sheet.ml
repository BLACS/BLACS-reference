open Cell
open Value
open Coordinates

module SheetMap = Map.Make(Coordinates)

open SheetMap

exception No_Dependencies

let empty = SheetMap.empty

let write time tag value coords sheet =
  try
    let c = find coords sheet in
    let c' = Cell.write time tag value c in
    (add coords c' sheet)
  with Not_found ->
    let cell = Cell.write time tag value Cell.empty in
    (add coords cell sheet)


let write_seq time tag origin width length values sheet =
  let f (sheet,coord) value =
    let s =  write time tag value coord sheet in
    (s, Coordinates.succ coord width length) in
  fst (List.fold_left f (sheet,origin) values)

let read time tag coords sheet =
  let c = find coords sheet in
  Cell.values time tag c

let read_seq disam time tag origin width length sheet =
  let sup = match origin with
      {row=r; col=c} ->
      coords (r+ pred width) (c+ pred length) in
  let p coords _ =
    lteq origin coords && lteq coords sup  in
  let m = filter p sheet in
  let b = bindings m in
  let values =
    function (coords,cell) ->
      (coords,Cell.value disam time tag cell) in
  List.map values b

let string_of_cell coords sv acc =
  let soi = string_of_int in
  let sc = (function {col=c; row=r} ->
      soi c ^ " " ^  soi r ^ ": ") coords in
  sc ^ sv ^ acc


let filter_coords p s =
  let m = filter p s in
  let b = bindings m in
  fst @@ List.split b

let dependencies formula s = match formula with 
    {ty=TyCount; data=[r1;c1;r2;c2;_]} ->
    let inf = coords r1 c1 in
    let sup = coords r2 (c2+1) in
    let p c _ = lteq inf c && lteq c sup in
    let l = filter_coords p s in
    l
  | _ -> raise No_Dependencies


let dependents disam time tag r0 c0 s =
  let p _ v = match Cell.value disam time tag v with
      {ty=TyCount; data=_} -> true
    | _ -> false in
  let m = filter p s in
  let coords_formulae = List.map (function (coords,cell) ->
      (coords,Cell.value disam time tag cell)) (bindings m) in
  let p' r1 c1 r2 c2 = (r1,c1) <= (r0,c0) && (r0,c0) <= (r2,c2) in
  let rec aux coords_formulae_list acc =
    match coords_formulae_list with
      (c, {ty=TyCount; data=[r1;c1;r2;c2;_]})::t
      when p' r1 c1 r2 c2  -> aux t (c::acc)
    |[] -> acc
    |_::t -> aux t acc in
  let l = aux coords_formulae [] in
  l

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
  let f c = (string_of_values @@ Cell.values time tag c) ^ " | " in 
  let ssheet = map f s in
  fold string_of_cell ssheet ""
