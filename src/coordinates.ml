type t = {row:int; col:int} [@@ deriving yojson]

let coords r c = {row=r; col=c}

let compare = fun
  {col=c1;row=r1} {col=c2;row=r2} ->
  Pervasives.compare (r1,c1) (r2,c2)

let lteq = fun 
  {col=c1;row=r1} {col=c2;row=r2} ->
  c1 <= c2 && r1 <= r2

let succ coord width length = match coord with
    {row=r; col=c} when c < length -> coords r (succ c)
  | {row=r; col=c} when r < width  -> coords (succ r) (c - pred length)
  | _ -> coord
