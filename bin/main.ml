open Core

module Project = struct
  module Update = struct
    type t =
      { status: string
      ; next_step: string
      ; created_at: int (* Time_ns.to_int_ns_since_epoch *)
      }
    [@@deriving yojson]
  end 

  type t = 
    { name: string
    ; updates: Update.t list }
end 

(*
   Store projects as directories like

   PROJECT_NAME/
     latest
     UPDATE_0_TIMESTAMP
     UPDATE_1_TIMESTAMP
     UPDATE_2_TIMESTAMP
     ...
*)

type 'h project_info =
  { name: string
  ; last_modified: int
  ; handle: 'h
  }

module type Database = sig
  type project_handle

  val create_project: string -> status:string -> next_step:string -> project_handle

  val projects : unit -> project_handle project_info list

  val input_update : unit -> Project.Update.t
end

type _ Effect.t += Eio_path : string -> Eio.Fs.dir Eio.Path.t Effect.t

let handle_eio_path (env: Eio.Stdenv.t) f =
  Effect.Deep.try_with f ()
    { effc= fun (type a) (e: a Effect.t) ->
          match e with
          | Eio_path s ->
            Some 
              (fun (k : (a, _) Effect.Deep.continuation) ->
                 Effect.Deep.continue k
                   Eio.Path.(env#fs / s))
          | _ -> None
    }

module Util = struct
  let split_string_exn s ~sub =
    let k = String.length sub in
    let i = Option.value_exn (String.substr_index s ~pattern:sub) in
    (String.sub s ~pos:0 ~len:i, String.sub s ~pos:(i + k) ~len:(String.length s - i - k))

  let last_occurrence c s =
    let rec go i =
      if i < 0
      then None
      else if Char.equal s.[i] c
      then Some i
      else go (i - 1)
    in 
    go (String.length s - 1)
end

let input_from_editor ~path ~initial_content =
  let eio_path = Effect.perform (Eio_path path) in
  (* TODO: Not sure the eio way to check existence *)
  (match Sys_unix.file_exists path with
   | `Yes  ->
     Eio.Path.(unlink eio_path) 
   | _ -> ());
  Eio.Path.save
    ~append:false
    ~create:(`If_missing 0o600)
    eio_path
    initial_content;
  if 
  Sys_unix.command 
    (Sys.getenv_exn "EDITOR" ^ " " ^ path)
  <> 0
  then failwith "Call to editor failed" ;
  Eio.Path.load eio_path

module Tern = struct
  let config_dir_path () = Sys.getenv_exn "HOME" ^/ ".tern"

  let config_dir = Memo.unit (fun () -> Effect.perform (Eio_path (config_dir_path ())))

  let projects_dir_name = "projects"

  let completed_dir_name = "completed"

  let projects_dir = Memo.unit (fun () -> Eio.Path.(config_dir () /projects_dir_name))

  let completed_dir = Memo.unit (fun () -> Eio.Path.(config_dir () / completed_dir_name))

  let init_dir d =
    match Sys_unix.is_directory d with
    | `No ->
      Eio.Path.mkdir ~perm:0o700 (Effect.perform (Eio_path d))
    | _ -> 
      ()

  let init_config_dir () =
    init_dir (config_dir_path ()) ;
    init_dir (config_dir_path () ^/ projects_dir_name);
    init_dir (config_dir_path () ^/ completed_dir_name)

  let complete project =
    Eio.Path.(
      rename
        (projects_dir () / project)
        (completed_dir () / project))

  type project_handle = Eio.Fs.dir Eio.Path.t

  let now () : int =Time_ns.(to_int_ns_since_epoch (now ()))

  let ( ^/ ) = Eio.Path.( / )

  let write_update ~(project: _ project_info) (u: Project.Update.t) =
    Eio.Path.save
      (project.handle ^/ Int.to_string u.created_at)
      (Yojson.Safe.to_string
         (Project.Update.to_yojson u ))
      ~create:(`If_missing 0o600)

  let project_path name = projects_dir () ^/ name

  let load_update path =
    match
      Eio.Path.load path
      |> Yojson.Safe.from_string
      |> Project.Update.of_yojson
    with
    | Ok u -> u
    | Error _ -> failwith "Corrupted database"

  let read_update ~(project: _ project_info) update_time : Project.Update.t =
    load_update
      (project.handle ^/ Int.to_string update_time)

  let input_update ?initialize () =
    let status = "Status:" in
    let next_step = "Next step:" in
    let parse (s : string) =
      let s =
        List.filter (String.split_lines s)
          ~f:(fun line -> String.length line = 0 || not (Char.equal line.[0] '#'))
        |> String.concat ~sep:"\n"
      in
      let a, b =
        String.chop_prefix_exn s ~prefix:status
        |> Util.split_string_exn ~sub:next_step
      in
      String.strip a, String.strip b
    in 
    let status, next_step = 
      input_from_editor
        ~path:"/tmp/TERN_UPDATE" 
        ~initial_content:(
          match initialize with
          | Some (u : Project.Update.t) ->
            status ^ "\n" ^ u.status ^ "\n" ^ next_step ^ "\n" ^ u.next_step
          | None ->
          status ^ "\n" ^ next_step ^ "\n" )
      |> parse
    in
    { Project.Update.next_step; status; created_at= now () }

  let update_path project time = project_path project ^/ Int.to_string (Time_ns.to_int_ns_since_epoch time)

  let update_project (name : string) (u: Project.Update.t) =
    let time = Int.to_string u.created_at in
    let path = project_path name in
    let project : _ project_info = {
          last_modified= u.created_at
        ; handle= path
        ; name }
    in
    write_update ~project u ;
    Eio.Path.save 
      (path ^/ "latest") 
      time 
      ~create:(`If_missing 0o600)

  let create_project (name : string) (u: Project.Update.t) =
    Eio.Path.mkdir ~perm:0o700 (project_path name) ;
    update_project name u

  let all_updates p =
    let project_path = project_path p in
    List.filter_map (Eio.Path.read_dir project_path) ~f:(fun f ->
        match f with
        | "latest" -> None
        | _ ->
          Some (load_update (project_path ^/ f)))

  let projects () =
    let c = projects_dir () in
    List.map (Eio.Path.read_dir c) ~f:(fun name ->
        let handle = c ^/ name in
          { name
          ; handle
          ; last_modified=
              Int.of_string (Eio.Path.load (handle ^/ "latest"))
          } )
end

(* Syntax is
   input: tern
   output: list of projects in order with status

   input: tern PROJECT_NAME
   output: status and next step

   input: tern PROJECT_NAME update
   output: goes into editor to write update

   tern PROJECT_NAME 
*)

let grid ~header rows =
  PrintBox.grid
    ~bars:true
    ~pad:(PrintBox.pad' ~col:1 ~lines:0)
    (Array.append
       [| Array.map header
            ~f:PrintBox.(text_with_style Style.(set_fg_color Green bold))
       |]
      rows)
    |> PrintBox.frame


let main () =
  let fail s =
    Out_channel.output_string stderr s;
    Out_channel.newline stderr;
    exit 1
  in 
  let projects = Memo.unit (fun () ->
      Eio.Path.read_dir (Tern.projects_dir ()))
  in 
  let parse_project p =
    match List.filter (projects ()) ~f:(fun q -> String.is_prefix q ~prefix:p) with
    | [] -> fail (sprintf "%s does not match any project" p)
    | [q] -> q
    | ps -> fail (sprintf "%s matches multiple projects:\n%s" p
                    (String.concat ~sep:"\n" ps))
  in
  let forbidden_names =
    let commands = [ "new" ; "update"; "completed" ] in
    String.Set.of_list
      (List.concat_map ~f:(fun s ->
           List.init (1 + String.length s) ~f:(fun i -> String.prefix s i))
        commands)
  in
  let time_string time =
    let s =
      Time_ns.to_string_trimmed
        ~zone:(Lazy.force Time_unix.Zone.local)
        (Time_ns.of_int_ns_since_epoch time) 
    in
    (* Chop off after (and including) the last colon,
        which is seconds and finer *)
    let i = Option.value_exn (Util.last_occurrence ':' s) in
    String.sub s ~pos:0 ~len:i
  in 
  match Sys.get_argv () with
  | [| _; ("help" | "-h" | "--help")|] ->
    print_endline {|Command options:
- NO ARGUMENTS: Show projects' status, sorted by date.
- new PROJECT_NAME: Create a new project.
- update PROJECT_NAME / PROJECT_NAME update: Update a given project.
- PROJECT_NAME: Display more detail about a given project.|}
  | [|_|] ->
    Tern.init_config_dir () ;
    let projects = Tern.projects () |> Array.of_list in
    Array.sort projects ~compare:(fun p1 p2 ->
        -(compare p1.last_modified p2.last_modified )) ;
    grid ~header:[| "Date"; "Project"; "Status"|]
      (Array.of_list_map (Tern.projects ()) ~f:(fun project ->
           let u = 
            Tern.read_update ~project
              project.last_modified 
           in
           [| PrintBox.text 
                (time_string project.last_modified)
            ; PrintBox.text project.name
            ; PrintBox.text u.status
           |]
         ))
    |> PrintBox_text.output stdout;
    Out_channel.newline stdout
  | [|_; ("n" | "ne" | "new"); name|] ->
    let update = Tern.input_update () in
    if Set.mem forbidden_names name
    then failwithf "Name \"%s\" not allowed" name () 
    else
    Tern.create_project name update
  | [| _; ("c" | "co" | "com" | "comp" | "compl" | "comple" | "complet" | "complete") ; p|] ->
    let p = parse_project p in
    Tern.complete p
  | [| _; ("u" | "up" | "upd" | "upda" | "updat" | "update") ; p|] ->
    let p = parse_project p in
    let last_update =
      let open Eio.Path in
      let p = Tern.project_path p  in
      let latest = load (p / "latest") in
      Tern.load_update (p / latest)
    in 
    let u = Tern.input_update ~initialize:last_update () in
    Tern.update_project p u
  | [| _; p |] ->
    let p = parse_project p in
    let updates = Tern.all_updates p |> Array.of_list in
    Array.sort updates
      ~compare:(fun u1 u2 -> -(compare u1.created_at u2.created_at)) ;
    grid ~header:[| "Date"; "Status"; "Next step"|]
      (Array.map updates ~f:(fun u ->
           Array.map ~f:PrintBox.text
           [| time_string u.created_at
            ; u.status
            ; u.next_step
           |]))
    |> PrintBox_text.output stdout;
    Out_channel.newline stdout
  | _ -> failwith "Unsupported command"

let () =
  Eio_main.run (fun env ->
    handle_eio_path env main )
