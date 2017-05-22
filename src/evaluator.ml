module Environment =
struct

  type 'a t = ((Coordinates.t * Time.t * string) * 'a) list

  exception UnboundKey
      
  let empty = []

  let lookup key env =
    try List.assoc key env
    with _ -> raise (UnboundKey)

  let bind key value env = (key,value)::env

  let to_list env = env

  let of_list l = l

end

open Value
open Environment

type color = Black| Grey | White

let eval coords time tag s env =
  try
    let f,(origin,width,length),disam,res,color =
      lookup (coords,time,tag) env                                in
    let seq = Sheet.read_seq disam time tag origin width length s in
    let origin',res' = List.fold_left f (origin,res) seq         in
    bind (coords,time,tag) (f,(origin',width,length),disam,res',White) env
  with
    _ -> env


let rec value  coords time tag s env =
  try
    let f,dom,disam,res,color = lookup (coords,time,tag) env in
    match color with
    | Black ->
      let env = bind (coords,time,tag) (f,dom,disam,res,Grey) env in
      let env' = eval coords time tag s env in
      value coords time tag s env'
    | Grey| White -> res 
  with
    _ -> none
  
let define disam coords time tag formula s env =
  match formula with
    {ty=TyNone  ; data=[] } -> env
  | {ty=TyInt   ; data=[i]} -> env
  | {ty=TyCount ; data=[r;c;width;length;v]} ->
    let origin              = Coordinates.coords r c    in
    let dom                 = (origin, width, length)   in
    let res                 = int 0                     in
    let f (origin,prev) cv  =
      match cv with
        (origin', {ty = TyInt   ; data = [i]}) when i = v ->
        (origin', Value.succ prev)
      | (origin', {ty = TyCount ; data = _  } )            ->
        let i = value origin' time tag  s env in
        if (i = int v)
        then (origin', Value.succ prev)
        else (origin', prev)
      | c,v                                               ->
        c,prev
    in
    bind (coords,time,tag) (f,dom,disam,res,Black) env
  | _ -> assert false
