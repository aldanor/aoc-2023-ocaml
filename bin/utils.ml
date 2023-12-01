open Unix
open Lwt
open Cohttp
open Cohttp_lwt_unix

let session_file = ".session"

let year_file = ".year"

let get_token () =
  let file = In_channel.open_text session_file in
  let token = In_channel.input_all file in
  In_channel.close file ; String.trim token

let get_year () =
  if Stdlib.Sys.file_exists year_file then (
    let file = In_channel.open_text year_file in
    let year = In_channel.input_all file in
    In_channel.close file ;
    int_of_string (String.trim year) )
  else
    let time = time () in
    let time = gmtime time in
    time.tm_year + 1900

let download_input day filename =
  let year = get_year () in
  let url =
    Printf.sprintf "https://adventofcode.com/%d/day/%s/input" year day
  in
  let token = get_token () in
  let headers = Header.init () in
  let headers = Header.add headers "Cookie" ("session=" ^ token) in
  let headers =
    Header.add headers "User-Agent" "github.com/aldanor/aoc-2023-ocaml"
  in
  let uri = Uri.of_string url in
  let body =
    Client.get ~headers uri
    >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    if code = 200 then body |> Cohttp_lwt.Body.to_string
    else failwith ("Unable to get data, status " ^ Int.to_string code)
  in
  let body = Lwt_main.run body in
  let body = String.trim body in
  let dir = Filename.dirname filename in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755 ;
  let file = Out_channel.open_text filename in
  Out_channel.output_string file body ;
  Out_channel.close file
