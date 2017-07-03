type t = {row:int; col:int} [@@ deriving yojson]

exception No_successor

let coords r c = {row=r; col=c}

let equal x y = match x,y with
    {row=xr; col=xc},{row=yr;col=yc} -> (xr,xc) = (yr,yc)

let hash = fun {row=r;col=c} ->
  Hashtbl.hash (r,c)

let compare = fun
  {col=c1;row=r1} {col=c2;row=r2} ->
  Pervasives.compare (r1,c1) (r2,c2)

let lteq = fun 
  {col=c1;row=r1} {col=c2;row=r2} ->
  c1 <= c2 && r1 <= r2

let succ origin x y p =
  let x0,y0 = origin.row,origin.col in
  match p with
    {row=a; col=b} when b < Pervasives.pred (y0+y) ->
    let a' = a in
    let b' = Pervasives.succ b in
    coords a' b'    
  | {row=a; col=b} when a < Pervasives.pred (x0+x) ->
    let a' = Pervasives.succ a in
    let b' = y0 in
    coords a' b'
  | _ -> raise No_successor


let to_string = fun {row=r; col=c} ->
  let soi = string_of_int in
  (soi r) ^ ":" ^ (soi c)
