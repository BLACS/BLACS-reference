type tylabel =
    TyInt [@name "int"]
  | TyNone [@name "none"]
  | TyCount [@name "count"] [@@deriving yojson]


type t =
  {ty:tylabel; data:int list} [@@deriving yojson]

let int i =  {ty = TyInt; data = [i]}

let count r1 c1 r2 c2 v = {ty = TyCount; data=[r1;c1;r2;c2;v]}

let none = {ty= TyNone; data=[]}

let string_of_value = function
    {ty = TyInt   ; data = [i]} -> "val "^ (string_of_int i)
  | {ty = TyCount ; data = [r1;c1;r2;c2;v]} ->
    let soi = string_of_int in
    "=#(" ^ soi r1 ^ ", " ^ soi c1 ^ ", " ^ soi r2 ^
    ", " ^ soi c2  ^ ", " ^ soi v  ^ ")"
  | {ty = TyNone  ; data =_}  -> "⊥"
  | _ -> assert false

let json_string_of_value = fun
  v -> to_yojson v |> Yojson.Safe.to_string 


let succ = function
    {ty = TyInt ; data = [i]} -> int (Pervasives.succ i)
  | _ -> assert false
