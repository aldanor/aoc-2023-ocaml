open Core
open Lwt
open Cohttp
open Cohttp_lwt_unix

let session_file, year_file = (".session", ".year")

let get_year () =
  if Sys_unix.file_exists_exn year_file then
    year_file |> In_channel.read_all |> String.strip |> int_of_string
  else
    Time_ns.now () |> Time_ns.to_date ~zone:Time_float.Zone.utc |> Date.year

let download_input day filename =
  let year = get_year () in
  printf "Downloading AoC %d input for day %02d...\n" year day ;
  let url = sprintf "https://adventofcode.com/%d/day/%d/input" year day in
  let token = session_file |> In_channel.read_all |> String.strip in
  let headers =
    Header.add_list (Header.init ())
      [ ("Cookie", "session=" ^ token)
      ; ("User-Agent", "github.com/aldanor/aoc-2023-ocaml") ]
  in
  let body =
    Client.get ~headers (Uri.of_string url)
    >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    if code = 200 then body |> Cohttp_lwt.Body.to_string
    else failwith (sprintf "Unable to get data, status %d" code)
  in
  let body = body |> Lwt_main.run |> String.strip in
  filename |> Filename.dirname |> Core_unix.mkdir_p ~perm:0o755 ;
  Out_channel.write_all filename ~data:body
