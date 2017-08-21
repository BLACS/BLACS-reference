open Eliom_lib
open Eliom_content
open Html.D
open Lwt
open Sheet
open Coordinates

module HT = Hashtbl

(*** Data types ***)

type sht              = Nativeint.t * Cell.t CellMap.CMap.t Sheet.SheetMap.t

type read_response    = string * string

type sheet_response   = (string, ((string, string) HT.t)) HT.t

type error            = {error_message : string}  [@@ deriving yojson]

(*** Values **)

let sheets: (string, sht) HT.t = HT.create 10

let sheet_responses: sheet_response = HT.create 10

let () = Random.self_init ()

let random_single_read ()  =
  let s = Sheet.empty in
  ignore (Sheet.read_seq
            None (fun x -> true) (Random.nativeint (Nativeint.of_int 10000)) "alice"
            (Coordinates.coords (Random.int 10000) (Random.int 10000)) 1 1 s)
let profile_random_single_read () =
  let t_0 = Unix.gettimeofday () in
  (for i=1 to 1000000 do
    random_single_read ();
  done);
  let t_1 = Unix.gettimeofday () in
  (t_1 -. t_0) /. 1000000.

let time_per_single_read = profile_random_single_read () 

(*** Services ***)

let main_service  =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()
    
let time_service  =
  Eliom_service.create
    ~path: (Eliom_service.Path ["time"])
    ~meth: (Eliom_service.Get Eliom_parameter.(suffix (string "name")))
    ()

let size_service =
  Eliom_service.create
    ~path: (Eliom_service.Path ["size"])
    ~meth: (Eliom_service.Get Eliom_parameter.(suffix (string "name")))
    ()

let read_response_service =
  Eliom_service.create
    ~path: (Eliom_service.Path [])
    ~meth: (Eliom_service.Get Eliom_parameter.(suffix (string "name" ** string "hash")))
    ()

let sheet_post_service path =
  Eliom_service.create
    ~path: (Eliom_service.Path [path])
    ~meth: (Eliom_service.Post (Eliom_parameter.(suffix (string "name")),
                                Eliom_parameter.raw_post_data ))
    ()

let read_service  = sheet_post_service "read"

let write_service = sheet_post_service "write"
    
(**** Handler helpers ****)
    
let json_mime_type = "application/json"

let send_json ~code json =
  Eliom_registration.String.send ~code (json, json_mime_type)

let send_error ~code error_message =
  let json = Yojson.Safe.to_string (error_to_yojson {error_message}) in
  send_json ~code json

let send_success () =
  Eliom_registration.String.send ~code:200 ("", json_mime_type)

let check_content_type ~mime_type content_type =
  match content_type with
  | Some ((type_, subtype), _)
      when (type_ ^ "/" ^ subtype) = mime_type -> true
  | _ -> false

let string_of_stream s =
  let open Ocsigen_stream in
  let buff = Buffer.create (16384)  in
  let rec aux s =
    next s >>= function
    | Finished _ -> Lwt.return buff
    | Cont (s,f) ->
      Buffer.add_string buff s; aux f
  in
  aux s >|= Buffer.contents

let read_post_body body =
  let content_stream = Ocsigen_stream.get body in
  string_of_stream content_stream

let hash name body =
  let open Cryptokit in
  let hex s = transform_string (Hexa.encode()) s in
  let s = name ^ body in
    hex (hash_string (Hash.sha2 256) s)

let get_sheet name =
    try
      HT.find sheets name
    with
      Not_found ->
      let t,s = Nativeint.zero,Sheet.empty in
      HT.add sheets name (t,s);
      t,s

let get_responses name =
    try
      HT.find sheet_responses name
    with
      Not_found ->
      let responses = HT.create 10 in
      HT.add sheet_responses name responses;
      responses

let filter_formulas lc =
  let def = Cell.((snd lc).definition) in
  let open Definition in
  def.ty = TyCount
  
let add_read_response name rrq hash =
  let filter =
    if ReadRequest.(rrq.filter_formulas)
    then Some filter_formulas
    else None
  in
  Lwt.return (
    let sheet = snd (get_sheet name) in
    let values = ReadRequest.(
      Sheet.read_seq
        filter
        (fun x -> true)
        rrq.time rrq.tag
        rrq.origin rrq.length
        rrq.width sheet)
    in
    let open LocatedCell in
    let values    = List.map located_cell values     in
    let json      = located_cell_list_to_json values in
    let responses = get_responses name in
    HT.add responses hash json)


(*** Handlers ***)

let main_handler () () =
  Eliom_content.Html.D.(
  Lwt.return
    (html
       (head (title (pcdata "BLACS")) [])
       (body [h1 [pcdata "Welcome to BLACS"]])))

let time_handler name () =
  let time,sheet = get_sheet name in
  HT.replace sheets name ((Nativeint.succ time), sheet);
  let json = Nativeint.to_string time in
  send_json ~code:200 json

let size_handler name () =
  let sheet = snd (get_sheet name)  in
  let dim   = Sheet.dimensions sheet in
  let json  = Dimensions.dimensions_to_json dim in
  send_json ~code:200 json

let read_response_handler (name,hash) () =
  try
    let responses = HT.find sheet_responses name in
    let response  = HT.find responses hash      in
    send_json ~code:200 response
  with
  Not_found -> send_error ~code:404 "Response not found"
    

let write_handler name (content_t, some_body) =
  if not (check_content_type ~mime_type:json_mime_type content_t)
  then
    send_error ~code:400 "Content-type is wrong, it must be JSON"
  else
    match some_body with
    | None ->
      send_error ~code:400 "Body content is missing"
    | Some (body) ->
      read_post_body body >>=
      fun str ->
        catch
          (fun () ->
             let wrq =
               match WriteRequest.write_request_of_json str with
                 Result.Ok x -> x
               | Result.Error e -> failwith e in
             let time,sheet = get_sheet name  in
             let sheet = WriteRequest.(
               Sheet.write_seq wrq.time wrq.tag wrq.origin
                 wrq.length wrq.width wrq.cells sheet)
             in
             HT.replace sheets name (time,sheet);
             send_success ())
          (fun _ -> send_error ~code:400 "Failure")
          
let read_handler name (content_t, some_body) =
  if not (check_content_type ~mime_type:json_mime_type content_t)
  then
    send_error ~code:401 "Content-type is wrong, it must be JSON"
  else
    match some_body with
    | None ->
      send_error ~code:402 "Body content is missing"
    | Some (body) ->
      read_post_body body >>=
      fun str ->
      catch
        (fun () ->
           let rrq =
             match ReadRequest.read_request_of_json str with
               Result.Ok(rrq) ->
               rrq
             | Result.Error(str) ->
               failwith str
           in
           let h = hash name str in
           let date = ReadRequest.(
             Unix.gettimeofday () +.
             (time_per_single_read ** 2.) *. (float_of_int rrq.width) *.
             (float_of_int rrq.length))                             in
           let%lwt  () = add_read_response name rrq h               in
           let rp   = ReadPromise.read_promise  ~date:date ~hash:h  in
           let json = ReadPromise.read_promise_to_json rp           in
           send_json ~code: 200 json)      
        (function
            | (Failure str) -> print_endline str; flush_all ();
              send_error ~code:403 "Failure"
            | _ -> send_error ~code:404 "Failure")
        
let () =
  Eliom_registration.Html.register main_service main_handler;
  
  Eliom_registration.Any.register  time_service time_handler;

  Eliom_registration.Any.register  size_service size_handler;

  Eliom_registration.Any.register  write_service write_handler;

  Eliom_registration.Any.register  read_service read_handler;

  Eliom_registration.Any.register  read_response_service read_response_handler



