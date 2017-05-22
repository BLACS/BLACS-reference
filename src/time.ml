type t = nativeint [@@ deriving yojson]


let time_ref= ref Nativeint.zero

let current () = !time_ref

let touch () =
  let t' = current () in
  time_ref := Nativeint.add t' Nativeint.one
