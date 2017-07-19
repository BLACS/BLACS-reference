open Cell

module Key = struct

  type t = Nativeint.t * string
           
  let compare = Pervasives.compare

end

module CMap = Map.Make(Key)

let empty = CMap.empty

let cells time tag cell_map   =
  let p k _ = fst k <= time in
  let m = CMap.filter p cell_map in
  try
    let latest_time =
      let  b = CMap.max_binding cell_map in
      fst (fst b) in
    try
      [(latest_time,tag),(CMap.find (latest_time,tag) m)]
    with Not_found ->
      let p k _ = (fst k) = latest_time in
      CMap.bindings (CMap.filter p m)
  with Not_found ->
    []

let cell predicate time tag cell_map =
  let vlist = cells time tag cell_map in
  let l =  List.filter (fun ((_,t),_) -> tag = t) vlist in
  try
    snd ((function [] -> List.hd (List.filter predicate vlist)
                 | h::_ -> h) l)
  with
    _ -> Cell.cell Definition.none

let write time tag cell cell_map =
  CMap.add (time,tag) cell cell_map

let diff tag t t' cell =
  let p k _ =
    let time_is_valid =
      t <= fst k && fst k <= t' in
    let tag_is_valid = (tag = (snd k))in
    let tag_not_defined =
      not @@ CMap.mem (fst k,tag) cell in
    time_is_valid &&
    (tag_is_valid || tag_not_defined) in
  let m = CMap.filter p cell in
  try
    CMap.bindings m
  with Not_found -> []


                      
