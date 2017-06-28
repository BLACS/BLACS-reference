type t = {row:int; col:int} [@@ deriving yojson]

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

let succ coord width length = match coord with
    {row=r; col=c} when c < (length-1) -> coords r (succ c)
  | {row=r; col=c} when r < (width-1)  -> coords (succ r) ((c mod length) - 1)
  | _ -> coord

let to_string = fun {row=r; col=c} ->
  let soi = string_of_int in
  (soi r) ^ ":" ^ (soi c)
