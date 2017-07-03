open Eliom_lib
open Eliom_content
open Html.D
open Lwt
open Sheet
open Coordinates

module HT = Hashtbl

(*** Data types ***)

type sht              = Nativeint.t * Value.t Cell.CellMap.t Sheet.SheetMap.t

type readResponse     = string * string

type sheetResponse    = (string, ((string, string) HT.t)) HT.t

type error            = {errorMessage : string}  [@@ deriving yojson]

type writeRQ          = {tag          : string        ;
                         time         : Nativeint.t   ;
                         origin       : Coordinates.t ;
                         length       : int           ;
                         width        : int           ;
                         values       : Value.t list  } [@@deriving yojson]

type readRQ           = {tag          : string        ;
                         time         : Nativeint.t   ;
                         origin       : Coordinates.t ;
                         width        : int           ;
                         length       : int           ;
                         default      : string option } [@@deriving yojson]

type locatedValue     = {coords       : Coordinates.t ;
                         value        : Value.t       } [@@deriving yojson]

type locatedValueList =  locatedValue list  [@@deriving yojson]

type readPromise      =  {date        : float         ;
                          hash        : string        } [@@deriving yojson]

let locatedValue      = fun (c,v)   -> { coords=c ; value=v  }

let readPromise       = fun (d,h) ->   { date = d ; hash = h }

(*** Value **)

let sheets: (string, sht) HT.t = HT.create 10

let sheetResponses: sheetResponse = HT.create 10

(*** Services ***)

let mainService  =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()
    
let timeService  =
  Eliom_service.create
    ~path: (Eliom_service.Path ["time"])
    ~meth: (Eliom_service.Get Eliom_parameter.(suffix (string "name")))
    ()

let readResponseService =
  Eliom_service.create
    ~path: (Eliom_service.Path [])
    ~meth: (Eliom_service.Get Eliom_parameter.(suffix (string "name" ** string "hash")))
    ()

let sheetPostService path =
  Eliom_service.create
    ~path: (Eliom_service.Path [path])
    ~meth: (Eliom_service.Post (Eliom_parameter.(suffix (string "name")),
                                Eliom_parameter.raw_post_data ))
    ()

let writeService = sheetPostService "write"

let readService  = sheetPostService "read"
    
(**** Handler helpers ****)
    
let jsonMimeType = "application/json"

let sendJson ~code json =
  Eliom_registration.String.send ~code (json, jsonMimeType)

let sendError ~code errorMessage =
  let json = Yojson.Safe.to_string (error_to_yojson {errorMessage}) in
  sendJson ~code json

let sendSuccess () =
  Eliom_registration.String.send ~code:200 ("", jsonMimeType)

let checkContentType ~mimeType contentType =
  match contentType with
  | Some ((type_, subtype), _)
      when (type_ ^ "/" ^ subtype) = mimeType -> true
  | _ -> false

let readPostBody ?(length = 4096) body =
  let contentStream = Ocsigen_stream.get body in
  Ocsigen_stream.string_of_stream length contentStream

let hash name body =
  let open Cryptokit in
  let hex s = transform_string (Hexa.encode()) s in
  let s = name ^ body in
    hex (hash_string (Hash.sha2 256) s)

let getSheet name =
    try
      HT.find sheets name
    with
      Not_found ->
      let t,s = Nativeint.zero,Sheet.empty in
      HT.add sheets name (t,s);
      t,s

let getResponses name =
    try
      HT.find sheetResponses name
    with
      Not_found ->
      let responses = HT.create 10 in
      HT.add sheetResponses name responses;
      responses

let addReadResponse name rrq hash =
  Lwt.return (
  let _,sheet = getSheet name in
  let values =
    Sheet.read_seq
      (fun x -> true)
      rrq.time rrq.tag
      rrq.origin rrq.width
      rrq.length sheet
  in
  let values = List.map locatedValue values      in
  let yojson = locatedValueList_to_yojson values in
  let json   = Yojson.Safe.to_string yojson      in
  let responses = getResponses name in
  HT.add responses hash json)

(*** Handlers ***)

let mainHandler () () =
  Eliom_content.Html.D.(
  Lwt.return
    (html
       (head (title (pcdata "BLACS")) [])
       (body [h1 [pcdata "Welcome to BLACS"]])))

let timeHandler name () =
  let time,sheet = getSheet name in
  HT.replace sheets name ((Nativeint.succ time), sheet);
  let json = Nativeint.to_string time in
  sendJson ~code:200 json

let readResponseHandler (name,hash) () =
  try
    let responses = HT.find sheetResponses name in
    let response  = HT.find responses hash      in
    sendJson ~code:200 response
  with
  Not_found -> sendError ~code:404 "Response not found"
    

let writeHandler name (contentType, someBody) =
  if not (checkContentType ~mimeType:jsonMimeType contentType)
  then
    sendError ~code:400 "Content-type is wrong, it must be JSON"
  else
    match someBody with
    | None ->
      sendError ~code:400 "Body content is missing"
    | Some (body) ->
      readPostBody body >>=
      fun str ->
        catch
          (fun () ->
             let yojson = Yojson.Safe.from_string str in
             let wrq    =
               match writeRQ_of_yojson yojson with
                 Result.Ok(wrq) -> wrq
               | Result.Error(s) ->
                 assert false
             in
             let time,sheet = getSheet name in
             let sheet =
               Sheet.write_seq wrq.time wrq.tag wrq.origin
                 wrq.width wrq.length wrq.values sheet
             in
             HT.replace sheets name (time,sheet);
             sendSuccess ())
          (fun _ -> sendError ~code:400 "Wbl")

let readHandler name (contentType, someBody) =
  if not (checkContentType ~mimeType:jsonMimeType contentType)
  then
    sendError ~code:400 "Content-type is wrong, it must be JSON"
  else
    match someBody with
    | None ->
      sendError ~code:400 "Body content is missing"
    | Some (body) ->
      readPostBody body >>=
      fun str ->
      catch
        (fun () ->
           let yojson = Yojson.Safe.from_string str        in
           let rrq =
             match readRQ_of_yojson yojson with
               Result.Ok(rrq) ->
               rrq
             | Result.Error(str) ->
               assert false
           in
           let h = hash name str in
           let date =
             Unix.gettimeofday () +.
             0.01 *. (float_of_int rrq.width) *.
             (float_of_int rrq.length)                                   in
           let%lwt  () = addReadResponse name rrq h                      in
           let rp   = readPromise (date,h)                               in
           let json = Yojson.Safe.(to_string (readPromise_to_yojson rp)) in
           sendJson ~code: 200 json)      
        (fun _ -> sendError ~code:400 "Wbl")
        
let () =
  Eliom_registration.Html.register mainService mainHandler;
  
  Eliom_registration.Any.register  timeService timeHandler;

  Eliom_registration.Any.register  writeService writeHandler;

  Eliom_registration.Any.register  readService readHandler;

  Eliom_registration.Any.register  readResponseService readResponseHandler



