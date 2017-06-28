open Value

module Key = struct

  type t = Nativeint.t * string
           
  let compare = Pervasives.compare

end

module CellMap = Map.Make(Key)

let empty = CellMap.empty

let values time tag cell   =
  let p k _ = fst k <= time in
  let m = CellMap.filter p cell in
  try
    let latest_time =
      let  b = CellMap.max_binding cell in
      fst (fst b) in
    try
      [(latest_time,tag),(CellMap.find (latest_time,tag) m)]
    with Not_found ->
      let p k _ = (fst k) = latest_time in
      CellMap.bindings (CellMap.filter p m)
  with Not_found ->
    []

let value predicate time tag cell =
  let vlist = values time tag cell in
  let l =  List.filter (fun ((_,t),_) -> tag = t) vlist in
  try
    snd ((function [] -> List.hd (List.filter predicate vlist)
                 | h::_ -> h) l)
  with
    _ -> Value.none

let write time tag value cell =
  CellMap.add (time,tag) value cell

let diff tag t t' cell =
  let p k _ =
    let time_is_valid =
      t <= fst k && fst k <= t' in
    let tag_is_valid = (tag = (snd k))in
    let tag_not_defined =
      not @@ CellMap.mem (fst k,tag) cell in
    time_is_valid &&
    (tag_is_valid || tag_not_defined) in
  let m = CellMap.filter p cell in
  try
    CellMap.bindings m
  with Not_found -> []


                      
