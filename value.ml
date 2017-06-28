type tylabel =
    TyInt
  | TyNone
  | TyCount 
    
let tylabel_to_yojson =
  let f s = `String s in
  function
    TyInt   -> f "int"
  | TyCount -> f "count"
  | TyNone  -> f "none"
                 
let tylabel_of_yojson =
  function
    `String "int"   -> Result.Ok(TyInt)
  | `String "count" -> Result.Ok(TyCount)
  | `String "none"  -> Result.Ok(TyNone)
  | _ -> Result.Error("tylabel")

type promise = {date:float; domain:int list; value: int option} [@@deriving yojson]
           
type t = {ty:tylabel; data:(int list) option; promises: (promise list) option} [@@deriving yojson]

let int i =  {ty = TyInt; data = Some [i]; promises = None}

let nullInt =  {ty = TyInt; data = None; promises = None}

let count r1 c1 r2 c2 v = {ty = TyCount; data=Some [r1;c1;r2;c2;v];promises=None}
                          
let none = {ty= TyNone; data=None; promises=None}
           
let string_of_value = function
    {ty = TyInt   ; data = Some [i]} -> "val "^ (string_of_int i)
  | {ty = TyInt   ; data = None    } -> "null"
  | {ty = TyCount ; data = Some [r1;c1;r2;c2;v]} ->
    let soi = string_of_int in
    "=#(" ^ soi r1 ^ ", " ^ soi c1 ^ ", " ^ soi r2 ^
    ", " ^ soi c2  ^ ", " ^ soi v  ^ ")"
  | {ty = TyNone  ; data =_}  -> "⊥"
  | _ -> assert false

let json_string_of_value = fun
  v -> to_yojson v |> Yojson.Safe.to_string 
                        
let succ = function
    {ty = TyInt ; data = Some [i]; promises:_} -> int (Pervasives.succ i)
  | _ -> assert false
